use crate::hir::{ExprKind, FunctionCall, Module, TypedExpr};
use crate::symbol::SymbolRegistry;
use crate::typing::function::Function;
use crate::typing::registry::FunctionId;
use crate::typing::user::{TypeId, UserType, ERROR_TYPE, UNKNOWN_TYPE};
use crate::typing::variable::VariableTable;
use crate::typing::{
    ascribe_type, lookup_path, lookup_type, Context, TypeChecker, TypeError, TypeErrorKind,
    TypeHint,
};
use crate::SourceLocation;
use ast::call::ProgrammaticCall;
use context::source::SourceSegmentHolder;

pub fn ascribe_pfc(
    call: &ProgrammaticCall,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let ty = lookup_path(
        &call.path,
        SymbolRegistry::Function,
        table,
        checker,
        ctx.modules,
        errors,
    );

    if ty.is_err() {
        return TypedExpr {
            kind: ExprKind::Noop,
            span: call.segment(),
            ty: ERROR_TYPE,
        };
    }

    let UserType::Function(function) = checker.types[ty] else {
        panic!(
            "function should have a function type {ty:?} {:?}",
            &checker.types[ty]
        );
    };

    ascribe_known_pfc(call, function, table, checker, storage, ctx, errors)
}

/// Generate IHR for a given Programmatic Function Call where the callee is forced to be the given `function_id` argument.
///
/// This function will ignore the pfc's path that it would normally use to retrieve the targeted function (as in [`ascribe_pfc`]),
/// and will try to match it with the given function
pub fn ascribe_known_pfc(
    ProgrammaticCall {
        path,
        arguments,
        type_parameters,
        segment: span,
    }: &ProgrammaticCall,
    function_id: FunctionId,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx @ Context { modules, hint, .. }: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let arguments = arguments
        .iter()
        .map(|expr| ascribe_type(expr, table, checker, storage, ctx, errors))
        .collect::<Vec<_>>();

    let mut type_parameters = type_parameters
        .iter()
        .map(|type_param| lookup_type(type_param, table, checker, modules, errors))
        .collect::<Vec<TypeId>>();

    let Function {
        ref declared_at,
        ref generic_variables,
        ref param_types,
        return_type,
        ..
    } = checker.registry[function_id];

    let mut return_type = return_type;
    if type_parameters.is_empty() && !generic_variables.is_empty() {
        // Try to infer the generic types from the actual arguments
        type_parameters = vec![UNKNOWN_TYPE; generic_variables.len()];
        for (arg, param) in arguments.iter().zip(param_types.iter()) {
            if let Some(generic_variable) = generic_variables.iter().position(|&ty| ty == param.ty)
            {
                if type_parameters[generic_variable] != UNKNOWN_TYPE
                    && type_parameters[generic_variable] != arg.ty
                {
                    errors.push(TypeError::new(
                        TypeErrorKind::TypeMismatch {
                            expected: checker.display(type_parameters[generic_variable]),
                            expected_due_to: None,
                            actual: checker.display(arg.ty),
                        },
                        SourceLocation::new(table.path().to_owned(), arg.span.clone()),
                    ));
                } else {
                    type_parameters[generic_variable] = arg.ty;
                }
            } else if let UserType::Parametrized {
                schema: param_schema,
                params: param_params,
                ..
            } = &checker.types[param.ty]
            {
                if let UserType::Parametrized {
                    schema,
                    params: arg_params,
                } = &checker.types[arg.ty]
                {
                    if schema == param_schema {
                        for param_param in param_params {
                            if let Some(idx) =
                                generic_variables.iter().position(|&ty| ty == *param_param)
                            {
                                type_parameters[idx].define_if_absent(arg_params[idx]);
                            }
                        }
                    }
                }
            }
        }
        if let TypeHint::Required(expected_return_ty) = hint {
            if let Some(idx) = generic_variables.iter().position(|&ty| ty == return_type) {
                type_parameters[idx].define_if_absent(expected_return_ty);
            } else if let UserType::Parametrized {
                schema: expected_schema,
                params: expected_params,
                ..
            } = &checker.types[expected_return_ty]
            {
                if let UserType::Parametrized {
                    schema,
                    params: fn_return_params,
                } = &checker.types[return_type]
                {
                    if schema == expected_schema {
                        // First, get the index of the generic_variables in the return_params list
                        for (fn_return_param, fn_actual) in
                            fn_return_params.iter().zip(expected_params)
                        {
                            if let Some(generic_idx) = generic_variables
                                .iter()
                                .position(|&ty| ty == *fn_return_param)
                            {
                                type_parameters[generic_idx].define_if_absent(*fn_actual);
                            }
                        }
                    }
                }
            }
        }
        if type_parameters.iter().any(|ty| *ty == UNKNOWN_TYPE) {
            errors.push(TypeError::new(
                TypeErrorKind::TypeAnnotationRequired {
                    types: generic_variables
                        .iter()
                        .map(|ty| checker.display(*ty))
                        .collect(),
                    insert_at: path
                        .last()
                        .expect("path should have at least one item")
                        .segment()
                        .end,
                },
                SourceLocation::new(table.path().to_owned(), span.clone()),
            ));
            return_type = ERROR_TYPE;
        }
    }

    if arguments.len() != param_types.len() {
        errors.push(TypeError::new(
            TypeErrorKind::ArityMismatch {
                expected: param_types.len(),
                received: arguments.len(),
            },
            SourceLocation::new(table.path().to_owned(), span.clone()),
        ));
    } else {
        for (arg, param) in arguments.iter().zip(param_types.iter()) {
            let param_ty = checker
                .types
                .concretize(param.ty, generic_variables, &type_parameters);
            if let Err(_) = checker.types.unify(arg.ty, param_ty) {
                errors.push(TypeError::new(
                    TypeErrorKind::TypeMismatch {
                        expected: checker.display(param_ty),
                        expected_due_to: Some(SourceLocation::new(
                            declared_at.clone(),
                            param.span.clone(),
                        )),
                        actual: checker.display(arg.ty),
                    },
                    SourceLocation::new(table.path().to_owned(), arg.span.clone()),
                ));
            }
        }
    }
    return_type = checker
        .types
        .concretize(return_type, generic_variables, &type_parameters);

    TypedExpr {
        kind: ExprKind::FunctionCall(FunctionCall {
            arguments,
            function_id,
        }),
        span: span.clone(),
        ty: return_type,
    }
}
