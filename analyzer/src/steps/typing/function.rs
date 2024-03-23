use std::collections::HashMap;
use std::fmt;

use ast::call::{MethodCall, ProgrammaticCall};
use ast::function::{FunctionDeclaration, FunctionParameter};
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation, SourceLocation};
use crate::environment::symbols::SymbolInfo;
use crate::reef::ReefId;
use crate::relations::{LocalId, ObjectId, SourceId, SymbolRef};
use crate::steps::typing::bounds::{apply_bounds, build_bounds, TypesBounds};
use crate::steps::typing::coercion::{
    convert_description, convert_expression, convert_many, resolve_type_annotation,
};
use crate::steps::typing::exploration::{Exploration, Links};
use crate::steps::typing::view::TypeInstanceVec;
use crate::steps::typing::{ascribe_types, ExpressionValue, TypingState};
use crate::types::engine::{Chunk, ChunkKind, FunctionId};
use crate::types::hir::{ExprKind, TypedExpr};
use crate::types::ty::{FunctionDesc, FunctionKind, MethodType, Parameter, Type, TypeRef};
use crate::types::{ERROR, STRING, UNIT};

/// An identified return during the exploration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Return {
    /// The returned type.
    pub(super) ty: TypeRef,

    /// The segment where the return is located.
    pub(super) segment: SourceSegment,
}

/// Identifies a function that correspond to a call.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct FunctionMatch {
    /// The converted arguments to pass to the function.
    ///
    /// If any conversion is required, it will be done here.
    pub(super) arguments: Vec<TypedExpr>,

    /// The function identifier to call.
    pub(super) function_id: FunctionId,
    /// Optional chunk identifier if this function has an associated source.
    pub(super) function_source: Option<SourceId>,

    /// The function return type.
    pub(super) return_type: TypeRef,

    /// The function's reef
    pub(super) reef: ReefId,
}

/// Gets the returned type of a function.
///
/// This verifies the type annotation if present against all the return types,
/// or try to guess the return type.
pub(super) fn infer_return(
    func: &FunctionDeclaration,
    expected_return_type: TypeRef,
    links: Links,
    typed_func_body: Option<&TypedExpr>,
    diagnostics: &mut Vec<Diagnostic>,
    exploration: &mut Exploration,
) -> TypeRef {
    if let Some(typed_func_body) = typed_func_body {
        let last = get_last_segment(typed_func_body);
        // If the last statement is a return, we don't need re-add it
        if exploration
            .returns
            .last()
            .map_or(true, |ret| ret.segment != last.segment)
            && last.ty.is_something()
            && last.ty.is_ok()
        {
            exploration.returns.push(Return {
                ty: typed_func_body.ty,
                segment: last.segment.clone(),
            });
        }
    }

    let mut typed_return_locations: Vec<_> = Vec::new();

    for ret in &exploration.returns {
        if convert_description(
            exploration,
            expected_return_type,
            ret.ty,
            &mut TypesBounds::inactive(),
            true,
        )
        .is_err()
        {
            typed_return_locations.push(Observation::here(
                links.source,
                exploration.externals.current,
                ret.segment.clone(),
                if func.return_type.is_some() {
                    format!(
                        "Found `{}`",
                        exploration.new_type_view(ret.ty, &TypesBounds::inactive())
                    )
                } else {
                    format!(
                        "Returning `{}`",
                        exploration.new_type_view(ret.ty, &TypesBounds::inactive())
                    )
                },
            ));
        }
    }

    if typed_return_locations.is_empty() {
        return expected_return_type;
    }

    if let Some(return_type_annotation) = func.return_type.as_ref() {
        diagnostics.push(
            Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch")
                .with_observations(typed_return_locations)
                .with_observation(Observation::context(
                    links.source,
                    exploration.externals.current,
                    return_type_annotation.segment(),
                    format!(
                        "Expected `{}` because of return type",
                        exploration.new_type_view(expected_return_type, &TypesBounds::inactive()),
                    ),
                )),
        );
        return ERROR;
    }

    let Some(body) = &func.body else {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::CannotInfer,
                "Function declaration needs explicit return type",
            )
            .with_observations(typed_return_locations)
            .with_help("Explicit the function's return type as it's not defined."),
        );

        return ERROR;
    };

    if matches!(body.as_ref(), Expr::Block(_)) {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::CannotInfer,
                "Return type is not inferred for block functions",
            )
            .with_observations(typed_return_locations)
            .with_help("Try adding an explicit return type to the function"),
        );

        return ERROR;
    }
    let segment = func.segment().start..body.segment().start;
    let types: Vec<_> = exploration.returns.iter().map(|ret| ret.ty).collect();
    let unify = convert_many(exploration, &mut TypesBounds::inactive(), types);

    if let Ok(common_type) = unify {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::CannotInfer,
                "Return type inference is not supported yet",
            )
            .with_observation(Observation::context(
                links.source,
                exploration.externals.current,
                segment,
                "No return type is specified",
            ))
            .with_observations(typed_return_locations)
            .with_help(format!(
                "Add -> {} to the function declaration",
                exploration.new_type_view(common_type, &TypesBounds::inactive()),
            )),
        );
    } else {
        diagnostics.push(
            Diagnostic::new(DiagnosticID::CannotInfer, "Failed to infer return type")
                .with_observation(Observation::context(
                    links.source,
                    exploration.externals.current,
                    segment,
                    "This function returns multiple types",
                ))
                .with_observations(typed_return_locations)
                .with_help("Try adding an explicit return type to the function"),
        );
    }
    ERROR
}

/// Ensures that the return type does not contains any reference to given type parameters of function.
fn check_for_leaked_type_parameters(
    exploration: &Exploration,
    not_to_leak: &[TypeRef],
    return_type: TypeRef,
    source: SourceId,
    call_segment: SourceSegment,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypeRef {
    let mut leaked_types = Vec::new();

    fn collect_leaked_types(
        exploration: &Exploration,
        not_to_leak: &[TypeRef],
        tpe: TypeRef,
        leaked_types: &mut Vec<TypeRef>,
    ) {
        if not_to_leak.contains(&tpe) {
            leaked_types.push(tpe)
        }
        let ty = exploration.get_type(tpe).unwrap();
        if let Type::Instantiated(base, params) = ty {
            collect_leaked_types(exploration, not_to_leak, *base, leaked_types);
            for param in params {
                collect_leaked_types(exploration, not_to_leak, *param, leaked_types);
            }
        }
    }

    collect_leaked_types(exploration, not_to_leak, return_type, &mut leaked_types);

    if let Some((first, tail)) = leaked_types.split_first() {
        let leaked_types_str = {
            tail.iter().fold(
                format!(
                    "`{}`",
                    exploration.new_type_view(*first, &TypesBounds::inactive())
                ),
                |acc, it| {
                    format!(
                        "{acc}, `{}`",
                        exploration.new_type_view(*it, &TypesBounds::inactive())
                    )
                },
            )
        };

        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::CannotInfer,
                "Cannot infer parameter types of function",
            )
            .with_observation(Observation::here(
                source,
                exploration.externals.current,
                call_segment,
                format!("please provide explicit types for generic parameters {leaked_types_str}"),
            )),
        );
        ERROR
    } else {
        return_type
    }
}

/// create a basic chunk from a function declaration
/// type its parameters, type parameters and return type
pub(super) fn declare_function(
    func: &FunctionDeclaration,
    exploration: &mut Exploration,
    function_links: Links,
    diagnostics: &mut Vec<Diagnostic>,
) -> Chunk {
    let mut type_params = Vec::new();
    let mut params = Vec::new();

    let func_source = function_links.source;
    exploration
        .ctx
        .init_locals(func_source, function_links.env().symbols.len());

    for (local_id, type_param) in func.type_parameters.iter().enumerate() {
        let param_type_id = exploration
            .typing
            .add_type(Type::Polytype, Some(type_param.name.to_string()));
        type_params.push(param_type_id);

        let param_type_ref = TypeRef::new(exploration.externals.current, param_type_id);
        exploration
            .ctx
            .set_local_typed(func_source, LocalId(local_id), param_type_ref);
        exploration
            .ctx
            .bind_name(type_param.name.to_string(), param_type_id);
    }

    let tparam_count = func.type_parameters.len();
    for (param_offset, param) in func.parameters.iter().enumerate() {
        let local_id = LocalId(tparam_count + param_offset);
        let param = type_parameter(local_id, exploration, param, function_links, diagnostics);
        exploration
            .ctx
            .set_local_typed(func_source, local_id, param.ty);
        params.push(param);
    }

    let return_type = func.return_type.as_ref().map_or(UNIT, |ty| {
        resolve_type_annotation(exploration, function_links, ty, diagnostics)
    });

    let function_id = exploration.type_engine.add_function(FunctionDesc {
        type_parameters: type_params,
        parameters: params,
        return_type,
        kind: FunctionKind::Function,
    });

    let function_type = exploration.typing.add_type(
        Type::Function(Some(func_source), function_id),
        Some(func.name.to_string()),
    );

    // The function body will be typed on next iteration
    Chunk {
        function_type,
        function_id,
        kind: func.body.as_ref().map_or(ChunkKind::DeclaredFunction, |_| {
            ChunkKind::DefinedFunction(None)
        }),
    }
}

/// Checks the type of a call expression.
pub(super) fn type_call(
    call: &ProgrammaticCall,
    exploration: &mut Exploration,
    links: Links,
    state: TypingState,
    diagnostics: &mut Vec<Diagnostic>,
) -> FunctionMatch {
    let arguments = &call.arguments;

    let call_symbol_ref = links.env().get_raw_symbol(call.segment()).unwrap();

    let (fun_reef, fun_origin, fun_local_id) = match call_symbol_ref {
        SymbolRef::Local(lid) => (exploration.externals.current, links.source, lid),
        SymbolRef::External(r) => {
            let call_symbol = links.relations[r].state.expect_resolved("unresolved");
            (call_symbol.reef, call_symbol.source, call_symbol.object_id)
        }
    };

    let function_type_ref = exploration
        .get_var(fun_origin, call_symbol_ref, links.relations)
        .unwrap()
        .type_ref;

    let (function_source, function_id) = match *exploration.get_type(function_type_ref).unwrap() {
        Type::Function(function_source, function_id) => (function_source, function_id),
        // We are (maybe) invoking a type's constructor.
        Type::Structure(structure_source, _)
        // check if the symbol kind is SymbolInfo::Type, otherwise, we are trying to call a function over a variable reference
        // that returns a structure, which is not something callable
        if exploration.get_symbol(fun_reef, fun_origin, fun_local_id, links).unwrap().ty == SymbolInfo::Type
        => {

            // there is only one constructor function for now (the default one)
            let constructor_id = exploration
                .get_methods(function_type_ref, "<init>")
                .unwrap()[0];
            (structure_source, constructor_id)
        }
        _ => {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    "Cannot invoke non function type",
                )
                    .with_observation(Observation::here(
                        links.source,
                        exploration.externals.current,
                        call.segment(),
                        format!(
                            "Call expression requires function, found `{}`",
                            exploration.new_type_view(function_type_ref, &TypesBounds::inactive())
                        ),
                    )),
            );

            let arguments = arguments
                .iter()
                .map(|expr| ascribe_types(exploration, links, diagnostics, expr, state))
                .collect::<Vec<_>>();

            return FunctionMatch {
                arguments,
                function_id: FunctionId(ObjectId::MAX),
                function_source: None,
                return_type: ERROR,
                reef: fun_reef,
            };
        }
    };

    let function = exploration.get_function(fun_reef, function_id).unwrap();
    let parameters = function.parameters.clone(); // TODO: avoid clone
    let return_type = function.return_type;

    if parameters.len() != arguments.len() {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::TypeMismatch,
                format!(
                    "This function takes {} {} but {} {} supplied",
                    parameters.len(),
                    pluralize(parameters.len(), "argument", "arguments"),
                    arguments.len(),
                    pluralize(arguments.len(), "was", "were"),
                ),
            )
            .with_observation(Observation::here(
                links.source,
                exploration.externals.current,
                call.segment.clone(),
                "Function is called here",
            )),
        );

        let arguments = arguments
            .iter()
            .map(|expr| ascribe_types(exploration, links, diagnostics, expr, state))
            .collect::<Vec<_>>();

        FunctionMatch {
            arguments,
            function_id,
            function_source,
            return_type: ERROR,
            reef: fun_reef,
        }
    } else {
        let types_parameters: Vec<_> = function
            .type_parameters
            .iter()
            .map(|type_id| TypeRef::new(fun_reef, *type_id))
            .collect();

        let expected_type = if let ExpressionValue::Expected(t) = state.local_value {
            Some(t)
        } else {
            None
        };

        let mut bounds = resolve_bounds(
            &call.type_parameters,
            fun_reef,
            None,
            function_id,
            expected_type,
            exploration,
            links,
            diagnostics,
        );

        let mut casted_arguments = Vec::with_capacity(parameters.len());
        for (param, arg) in parameters.iter().cloned().zip(arguments) {
            let param_bound = bounds.get_bound(param.ty);

            let arg = ascribe_types(
                exploration,
                links,
                diagnostics,
                arg,
                state.with_local_value(ExpressionValue::Expected(param_bound)),
            );

            let casted_argument = convert_expression(
                arg,
                param_bound,
                &mut bounds,
                exploration,
                links.source,
                diagnostics,
            );

            let casted_argument = match casted_argument {
                Ok(arg) => {
                    bounds.update_bounds(param.ty, arg.ty, exploration);
                    arg
                }
                Err(arg) => {
                    diagnostics.push(diagnose_arg_mismatch(
                        exploration,
                        links.source,
                        exploration.externals.current,
                        fun_reef,
                        &param,
                        &arg,
                        &bounds,
                    ));
                    arg
                }
            };

            casted_arguments.push(casted_argument);
        }

        let return_type = apply_bounds(exploration, return_type, &bounds);

        let return_type = check_for_leaked_type_parameters(
            exploration,
            &types_parameters,
            return_type,
            links.source,
            call.segment(),
            diagnostics,
        );

        FunctionMatch {
            arguments: casted_arguments,
            function_id,
            function_source,
            return_type,
            reef: fun_reef,
        }
    }
}

/// update given bounds to update type parameters bounds of the function's return type from the given hint
pub(super) fn infer_return_from_hint(
    exploration: &Exploration,
    return_type: TypeRef,
    return_type_hint: TypeRef,
    bounds: &mut HashMap<TypeRef, TypeRef>,
) {
    let return_tpe = exploration.get_type(return_type).unwrap();
    let hint_tpe = exploration.get_type(return_type_hint).unwrap();
    match (return_tpe, hint_tpe) {
        (Type::Polytype, _) => {
            bounds.insert(return_type, return_type_hint);
        }
        (Type::Instantiated(_, return_params), Type::Instantiated(_, hint_params)) => {
            for (return_param, hint_param) in return_params.iter().zip(hint_params) {
                infer_return_from_hint(exploration, *return_param, *hint_param, bounds)
            }
        }
        _ => {}
    }
}

#[allow(clippy::too_many_arguments)]
fn resolve_bounds(
    user_bounds: &[ast::r#type::Type],
    declaration_reef: ReefId,
    obj: Option<TypeRef>,
    function_id: FunctionId,
    return_hint: Option<TypeRef>,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypesBounds {
    let bounds_types = user_bounds
        .iter()
        .map(|ty| resolve_type_annotation(exploration, links, ty, diagnostics))
        .collect::<Vec<_>>();
    let bounds = build_bounds(
        &bounds_types,
        obj,
        declaration_reef,
        function_id,
        return_hint,
        exploration,
    );
    let entry = exploration
        .get_function(declaration_reef, function_id)
        .unwrap();

    let expected_tparams_count = entry.type_parameters.len();
    if !user_bounds.is_empty() && user_bounds.len() != expected_tparams_count {
        diagnostics.push(diagnose_wrong_tparams_count(
            user_bounds,
            expected_tparams_count,
            links,
            exploration.externals.current,
        ));
    }

    bounds
}

fn diagnose_wrong_tparams_count(
    user_tparams: &[ast::r#type::Type],
    expected_count: usize,
    links: Links,
    reef: ReefId,
) -> Diagnostic {
    let first = user_tparams.first().unwrap();
    let last = user_tparams.last().unwrap();

    let segment = first.segment().start..last.segment().end;

    Diagnostic::new(
        DiagnosticID::InvalidTypeArguments,
        "Wrong type argument count",
    )
    .with_observation(Observation::here(
        links.source,
        reef,
        segment,
        format!(
            "`{}` type parameter specified, expected `{}`.",
            user_tparams.len(),
            expected_count
        ),
    ))
}

/// A specialized [`crate::types::hir::MethodCall`] between two expressions.
pub(super) struct BinaryMethodMatch {
    pub(crate) left: TypedExpr,
    pub(crate) right: TypedExpr,
    pub(crate) return_type: TypeRef,
    pub(crate) function_id: FunctionId,
    pub(crate) reef: ReefId,
}

impl From<BinaryMethodMatch> for crate::types::hir::MethodCall {
    fn from(binary: BinaryMethodMatch) -> Self {
        Self {
            callee: Box::new(binary.left),
            arguments: vec![binary.right],
            function_id: binary.function_id,
        }
    }
}

/// Checks the type of a method expression.
pub(super) fn find_operand_implementation(
    exploration: &Exploration,
    reef: ReefId,
    methods: &[FunctionId],
    left: TypedExpr,
    right: TypedExpr,
) -> Result<BinaryMethodMatch, TypedExpr> {
    for method_id in methods {
        let method = exploration.get_function(reef, *method_id).unwrap();
        if let [param] = &method.parameters.as_slice() {
            if param.ty == right.ty {
                let return_type = exploration.concretize(method.return_type, left.ty);
                return Ok(BinaryMethodMatch {
                    left,
                    right,
                    function_id: *method_id,
                    return_type,
                    reef,
                });
            }
        }
    }
    Err(left.poison())
}

/// Creates a list of the type parameters of methods.
pub(super) fn list_operator_defined_for<'a>(
    exploration: &'a Exploration,
    methods: &[&MethodType],
    bounds: &'a TypesBounds,
) -> TypeInstanceVec<'a> {
    let types = methods
        .iter()
        .flat_map(|method| {
            if let [param] = method.parameters.as_slice() {
                Some(param.ty)
            } else {
                None
            }
        })
        .collect();
    TypeInstanceVec::new(types, exploration, bounds)
}

/// Checks the type of a method expression.
#[allow(clippy::too_many_arguments)]
pub(super) fn type_method(
    method_call: &MethodCall,
    callee: &TypedExpr,
    links: Links,
    arguments: Vec<TypedExpr>,
    diagnostics: &mut Vec<Diagnostic>,
    exploration: &mut Exploration,
    source: SourceId,
    return_hint: Option<TypeRef>,
) -> Option<FunctionMatch> {
    if callee.ty.is_err() {
        return None;
    }

    let type_args: Vec<_> = method_call
        .type_parameters
        .iter()
        .map(|t| resolve_type_annotation(exploration, links, t, diagnostics))
        .collect();

    let current_reef = exploration.externals.current;

    // Directly callable types just have a single method called `apply`
    let method_name = method_call
        .name
        .as_ref()
        .map(|name| name.value.as_str())
        .unwrap_or("apply");
    let type_methods = exploration.get_methods(callee.ty, method_name);
    if type_methods.is_none() {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::UnknownMethod,
                if method_call.name.is_some() {
                    format!(
                        "No method named `{method_name}` found for type `{}`",
                        exploration.new_type_view(callee.ty, &TypesBounds::inactive())
                    )
                } else {
                    format!(
                        "Type `{}` is not directly callable",
                        exploration.new_type_view(callee.ty, &TypesBounds::inactive())
                    )
                },
            )
            .with_observation((source, current_reef, method_call.segment.clone()).into()),
        );
        return None;
    }

    let methods = type_methods.unwrap(); // We just checked for None

    let result = find_exact_method(
        callee.ty,
        methods,
        &arguments,
        &type_args,
        return_hint,
        exploration,
    );

    let method_base_reef = exploration.get_base_type(callee.ty).reef;

    if let Some((method_id, bounds)) = result {
        let method = exploration
            .get_function(method_base_reef, method_id)
            .unwrap();

        let types_parameters: Vec<_> = method
            .type_parameters
            .iter()
            .map(|type_id| TypeRef::new(method_base_reef, *type_id))
            .collect();

        let return_type = exploration.concretize(method.return_type, callee.ty);
        let return_type = apply_bounds(exploration, return_type, &bounds);
        let return_type = check_for_leaked_type_parameters(
            exploration,
            &types_parameters,
            return_type,
            links.source,
            method_call.segment(),
            diagnostics,
        );

        // We have an exact match
        return Some(FunctionMatch {
            arguments,
            function_id: method_id,
            function_source: None,
            return_type,
            reef: callee.ty.reef,
        });
    }

    if methods.len() == 1 {
        // If there is only one method, we can give a more specific error by adding
        // an observation for each invalid type
        let method_id = *methods.first().unwrap();
        let method = exploration
            .get_function(method_base_reef, method_id)
            .unwrap();

        if method.parameters.len() != arguments.len() {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    format!(
                        "This method takes {} {} but {} {} supplied",
                        method.parameters.len(),
                        pluralize(method.parameters.len(), "argument", "arguments"),
                        arguments.len(),
                        pluralize(arguments.len(), "was", "were")
                    ),
                )
                .with_observation(Observation::here(
                    source,
                    current_reef,
                    method_call.segment(),
                    "Method is called here",
                ))
                .with_help(format!(
                    "The method signature is `{}::{}`",
                    exploration.new_type_view(callee.ty, &TypesBounds::inactive()),
                    Signature::new(exploration, method_name, method)
                )),
            );
        } else {
            let mut bounds = resolve_bounds(
                &method_call.type_parameters,
                method_base_reef,
                Some(callee.ty),
                method_id,
                return_hint,
                exploration,
                links,
                diagnostics,
            );

            // mutable borrow of `exploration` in  `resolve_bounds` call
            // forces us to retrieve the method once again to drop previous
            // immutable borrow of `exploration`, that lives through the `method` var.
            let methods = exploration.get_methods(callee.ty, method_name).unwrap();
            let method_id = *methods.first().unwrap();
            let method = exploration
                .get_function(method_base_reef, method_id)
                .unwrap();

            for (param, arg) in method.parameters.iter().zip(arguments.iter()) {
                let param_bound = bounds.get_bound(param.ty);

                match convert_description(exploration, param_bound, arg.ty, &mut bounds, true) {
                    Ok(ty) => {
                        bounds.update_bounds(param.ty, ty, exploration);
                    }
                    Err(_) => {
                        let param = Parameter {
                            location: param.location.clone(),
                            ty: param_bound,
                            local_id: param.local_id,
                        };
                        let diagnostic = diagnose_arg_mismatch(
                            exploration,
                            source,
                            current_reef,
                            callee.ty.reef,
                            &param,
                            arg,
                            &bounds,
                        )
                        .with_observation(Observation::here(
                            source,
                            current_reef,
                            if let Some(name) = &method_call.name {
                                name.segment()
                            } else {
                                method_call.segment()
                            },
                            "Arguments to this method are incorrect",
                        ));
                        diagnostics.push(diagnostic);
                    }
                }
            }
        }
    } else {
        // If there are multiple methods, list them all
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::UnknownMethod,
                format!(
                    "No matching method found for `{method_name}::{}`",
                    exploration.new_type_view(callee.ty, &TypesBounds::inactive())
                ),
            )
            .with_observation(Observation::here(
                source,
                current_reef,
                method_call.segment(),
                "Method is called here",
            )),
        );
    }
    None
}

/// Generates a type mismatch between a parameter and an argument.
fn diagnose_arg_mismatch(
    exploration: &Exploration,
    source: SourceId,
    current_reef: ReefId,
    param_reef: ReefId,
    param: &Parameter,
    arg: &TypedExpr,
    bounds: &TypesBounds,
) -> Diagnostic {
    let diagnostic = Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch").with_observation(
        Observation::here(
            source,
            current_reef,
            arg.segment.clone(),
            format!(
                "Expected `{}`, found `{}`",
                exploration.new_type_view(param.ty, bounds),
                exploration.new_type_view(arg.ty, bounds)
            ),
        ),
    );
    if let Some(location) = &param.location {
        diagnostic.with_observation(Observation::context(
            location.source,
            param_reef,
            location.segment.clone(),
            "Parameter is declared here",
        ))
    } else {
        diagnostic
    }
}

/// Finds a matching method for the given arguments.
pub(super) fn find_exact_method(
    obj: TypeRef,
    methods: &[FunctionId],
    args: &[TypedExpr],
    type_args: &[TypeRef],
    return_hint: Option<TypeRef>,
    exploration: &Exploration,
) -> Option<(FunctionId, TypesBounds)> {
    let obj_type_reef = exploration.get_base_type(obj).reef;

    'methods: for method_id in methods {
        let method = exploration.get_function(obj_type_reef, *method_id).unwrap();
        if method.parameters.len() != args.len() {
            continue;
        }

        let mut bounds = build_bounds(
            type_args,
            Some(obj),
            obj_type_reef,
            *method_id,
            return_hint,
            exploration,
        );

        for (param, arg) in method.parameters.iter().zip(args.iter()) {
            let param_ty = exploration.concretize(param.ty, obj);
            let param_bound = bounds.get_bound(param_ty);

            let converted =
                convert_description(exploration, param_bound, arg.ty, &mut bounds, true);
            match converted {
                Ok(ty) => {
                    bounds.update_bounds(param.ty, ty, exploration);
                }
                Err(_) => continue 'methods,
            }
        }
        return Some((*method_id, bounds));
    }
    None
}

/// Type check a single function parameter.
pub(super) fn type_parameter(
    local_id: LocalId,
    exploration: &mut Exploration,
    param: &FunctionParameter,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
) -> Parameter {
    match param {
        FunctionParameter::Named(named) => {
            let type_id = named.ty.as_ref().map_or(STRING, |ty| {
                resolve_type_annotation(exploration, links, ty, diagnostics)
            });
            Parameter {
                location: Some(SourceLocation::new(
                    links.source,
                    exploration.externals.current,
                    named.segment(),
                )),
                ty: type_id,
                local_id,
            }
        }
        FunctionParameter::Slf(_) => todo!("method not supported yet"),
        FunctionParameter::Variadic(_, _) => todo!("Arrays are not supported yet"),
    }
}

fn get_last_segment(expr: &TypedExpr) -> &TypedExpr {
    match &expr.kind {
        ExprKind::Block(expressions) => expressions.last().map_or(expr, get_last_segment),
        _ => expr,
    }
}

fn pluralize<'a>(count: usize, singular: &'a str, plural: &'a str) -> &'a str {
    if count == 1 {
        singular
    } else {
        plural
    }
}

/// A formatted signature of a function.
struct Signature<'a> {
    exploration: &'a Exploration<'a>,
    name: &'a str,
    function: &'a FunctionDesc,
}

impl<'a> Signature<'a> {
    /// Creates a new signature.
    fn new(exploration: &'a Exploration<'a>, name: &'a str, function: &'a FunctionDesc) -> Self {
        Self {
            exploration,
            name,
            function,
        }
    }
}

impl fmt::Display for Signature<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.name)?;
        if let Some((first, parameters)) = self.function.parameters.split_first() {
            write!(
                f,
                "{}",
                self.exploration
                    .new_type_view(first.ty, &TypesBounds::inactive())
            )?;
            for param in parameters {
                write!(
                    f,
                    ", {}",
                    self.exploration
                        .new_type_view(param.ty, &TypesBounds::inactive())
                )?;
            }
        }
        if self.function.return_type.is_nothing() {
            write!(f, ")")
        } else {
            write!(
                f,
                ") -> {}",
                self.exploration
                    .new_type_view(self.function.return_type, &TypesBounds::inactive())
            )
        }
    }
}
