use ast::call::{MethodCall, ProgrammaticCall};
use ast::function::{FunctionDeclaration, FunctionParameter};
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation, SourceLocation};
use crate::reef::{ReefId, Reefs};
use crate::relations::{Definition, SourceId, SymbolRef};
use crate::steps::typing::coercion::convert_expression;
use crate::steps::typing::exploration::{Exploration, UniversalReefAccessor};
use crate::steps::typing::TypingState;
use crate::types::hir::{ExprKind, TypedExpr};
use crate::types::ty::{FunctionType, MethodType, Parameter, Type, TypeRef};
use crate::types::{
    convert_description, convert_many, get_type, resolve_type, ERROR, STRING, UNIT,
};

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
    pub(super) definition: Definition,

    /// The function return type.
    pub(super) return_type: TypeRef,
}

/// Gets the returned type of a function.
///
/// This verifies the type annotation if present against all the return types,
/// or try to guess the return type.
pub(super) fn infer_return(
    func: &FunctionDeclaration,
    typed_func: &TypedExpr,
    diagnostics: &mut Vec<Diagnostic>,
    exploration: &mut Exploration,
    reefs: &Reefs,
    state: TypingState,
) -> TypeRef {
    let last = get_last_segment(typed_func);
    // If the last statement is a return, we don't need re-add it
    if exploration
        .returns
        .last()
        .map_or(true, |ret| ret.segment != last.segment)
        && last.ty.is_something()
        && last.ty.is_ok()
    {
        exploration.returns.push(Return {
            ty: typed_func.ty,
            segment: last.segment.clone(),
        });
    }
    let ura = exploration.universal_accessor(state.reef, reefs);

    let expected_return_type = if let Some(return_type_annotation) = func.return_type.as_ref() {
        // An explicit return type is present, check it against all the return types.
        resolve_type(&ura, state.reef, state.source, return_type_annotation)
    } else {
        UNIT
    };

    let mut typed_return_locations: Vec<_> = Vec::new();

    for ret in &exploration.returns {
        if convert_description(&ura, expected_return_type, ret.ty).is_err() {
            typed_return_locations.push(Observation::here(
                state.source,
                ret.segment.clone(),
                if func.return_type.is_some() {
                    format!("Found `{}`", get_type(ret.ty, &ura).unwrap())
                } else {
                    format!("Returning `{}`", get_type(ret.ty, &ura).unwrap())
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
                    state.source,
                    return_type_annotation.segment(),
                    format!(
                        "Expected `{}` because of return type",
                        get_type(expected_return_type, &ura).unwrap()
                    ),
                )),
        );
    } else if !matches!(func.body.as_ref(), Expr::Block(_)) {
        let segment = func.segment().start..func.body.segment().start;
        let unify = convert_many(&ura, exploration.returns.iter().map(|ret| ret.ty));
        if let Ok(common_type) = unify {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::CannotInfer,
                    "Return type inference is not supported yet",
                )
                .with_observation(Observation::context(
                    state.source,
                    segment,
                    "No return type is specified",
                ))
                .with_observations(typed_return_locations)
                .with_help(format!(
                    "Add -> {} to the function declaration",
                    get_type(common_type, &ura).unwrap()
                )),
            );
        } else {
            diagnostics.push(
                Diagnostic::new(DiagnosticID::CannotInfer, "Failed to infer return type")
                    .with_observation(Observation::context(
                        state.source,
                        segment,
                        "This function returns multiple types",
                    ))
                    .with_observations(typed_return_locations)
                    .with_help("Try adding an explicit return type to the function"),
            );
        }
    } else {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::CannotInfer,
                "Return type is not inferred for block functions",
            )
            .with_observations(typed_return_locations)
            .with_help("Try adding an explicit return type to the function"),
        );
    }
    ERROR
}

/// Checks the type of a call expression.
pub(super) fn type_call(
    call: &ProgrammaticCall,
    arguments: Vec<TypedExpr>,
    diagnostics: &mut Vec<Diagnostic>,
    ura: &UniversalReefAccessor,
    state: TypingState,
) -> FunctionMatch {
    let engine = ura.get_engine(state.reef).unwrap();
    let relations = ura.get_relations(state.reef).unwrap();
    let env = engine.get_environment(state.source).unwrap();

    let call_symbol_ref = env.get_raw_symbol(call.segment()).unwrap();

    let fun_reef = match call_symbol_ref {
        SymbolRef::Local(_) => state.reef,
        SymbolRef::External(r) => {
            let call_symbol = relations[r].state.expect_resolved("unresolved");
            call_symbol.reef
        }
    };

    let fun_reef_relations = ura.get_relations(fun_reef).unwrap();

    let type_ref = ura
        .get_types(fun_reef)
        .unwrap()
        .context
        .get(fun_reef_relations, state.source, call_symbol_ref)
        .unwrap()
        .type_ref;

    match get_type(type_ref, ura).unwrap() {
        Type::Function(declaration) => {
            let declaration = *declaration;
            let entry = ura
                .get_types(fun_reef)
                .unwrap()
                .engine
                .get(declaration)
                .unwrap();
            let parameters = entry.parameters();
            let return_type = entry.return_type();
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
                        state.source,
                        call.segment.clone(),
                        "Function is called here",
                    )),
                );
                FunctionMatch {
                    arguments,
                    definition: Definition::error(),
                    return_type,
                }
            } else {
                let mut casted_arguments = Vec::with_capacity(parameters.len());
                for (param, arg) in parameters.iter().zip(arguments) {
                    casted_arguments.push(
                        match convert_expression(arg, param.ty, state, ura, diagnostics) {
                            Ok(arg) => arg,
                            Err(arg) => {
                                diagnostics.push(diagnose_arg_mismatch(
                                    ura,
                                    state.source,
                                    param,
                                    &arg,
                                ));
                                arg
                            }
                        },
                    );
                }
                FunctionMatch {
                    arguments: casted_arguments,
                    definition: declaration,
                    return_type,
                }
            }
        }
        ty => {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    "Cannot invoke non function type",
                )
                .with_observation(Observation::here(
                    state.source,
                    call.segment(),
                    format!("Call expression requires function, found `{ty}`"),
                )),
            );
            FunctionMatch {
                arguments,
                definition: Definition::error(),
                return_type: ERROR,
            }
        }
    }
}

/// Checks the type of a method expression.
pub(super) fn find_operand_implementation<'a>(
    methods: &'a [MethodType],
    right: &TypedExpr,
) -> Option<&'a MethodType> {
    for method in methods {
        if method.parameters.len() != 1 {
            continue;
        }
        if let Some(ty) = method.parameters.first() {
            if ty.ty == right.ty {
                return Some(method);
            }
        }
    }
    None
}

/// Checks the type of a method expression.
pub(super) fn type_method<'a>(
    method_call: &MethodCall,
    callee: &TypedExpr,
    arguments: &[TypedExpr],
    diagnostics: &mut Vec<Diagnostic>,
    ura: &'a UniversalReefAccessor,
    state: TypingState,
) -> Option<&'a MethodType> {
    if callee.ty.is_err() {
        return None;
    }

    // Directly callable types just have a single method called `apply`
    let method_name = method_call.name.unwrap_or("apply");
    let type_reef_types = ura.get_types(callee.ty.reef).unwrap();

    let type_methods = type_reef_types
        .engine
        .get_methods(callee.ty.type_id, method_name);
    if type_methods.is_none() {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::UnknownMethod,
                if method_call.name.is_some() {
                    format!(
                        "No method named `{method_name}` found for type `{}`",
                        get_type(callee.ty, ura).unwrap()
                    )
                } else {
                    format!(
                        "Type `{}` is not directly callable",
                        get_type(callee.ty, ura).unwrap()
                    )
                },
            )
            .with_observation((state.source, method_call.segment.clone()).into()),
        );
        return None;
    }

    let methods = type_methods.unwrap(); // We just checked for None
    let method = find_exact_method(methods, arguments);
    if let Some(method) = method {
        // We have an exact match
        return Some(method);
    }

    if methods.len() == 1 {
        // If there is only one method, we can give a more specific error by adding
        // an observation for each invalid type
        let method = methods.first().unwrap();
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
                    state.source,
                    method_call.segment(),
                    "Method is called here",
                ))
                .with_help(format!(
                    "The method signature is `{}::{}`",
                    get_type(callee.ty, ura).unwrap(),
                    signature_to_string(method_name, method, ura)
                )),
            );
        } else {
            for (param, arg) in method.parameters.iter().zip(arguments.iter()) {
                if convert_description(ura, param.ty, arg.ty).is_err() {
                    let diagnostic = diagnose_arg_mismatch(ura, state.source, param, arg)
                        .with_observation(Observation::here(
                            state.source,
                            method_call.segment(),
                            "Arguments to this method are incorrect",
                        ));
                    diagnostics.push(diagnostic);
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
                    get_type(callee.ty, ura).unwrap()
                ),
            )
            .with_observation(Observation::here(
                state.source,
                method_call.segment(),
                "Method is called here",
            )),
        );
    }
    None
}

/// Generates a type mismatch between a parameter and an argument.
fn diagnose_arg_mismatch(
    ura: &UniversalReefAccessor,
    source: SourceId,
    param: &Parameter,
    arg: &TypedExpr,
) -> Diagnostic {
    let diagnostic = Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch").with_observation(
        Observation::here(
            source,
            arg.segment.clone(),
            format!(
                "Expected `{}`, found `{}`",
                get_type(param.ty, ura).unwrap(),
                get_type(arg.ty, ura).unwrap()
            ),
        ),
    );
    if let Some(location) = &param.location {
        diagnostic.with_observation(Observation::context(
            location.source,
            location.segment.clone(),
            "Parameter is declared here",
        ))
    } else {
        diagnostic
    }
}

/// Find a matching method for the given arguments.
fn find_exact_method<'a>(methods: &'a [MethodType], args: &[TypedExpr]) -> Option<&'a MethodType> {
    for method in methods {
        if method.parameters.len() != args.len() {
            continue;
        }
        let mut matches = true;
        for (param, arg) in method.parameters.iter().zip(args.iter()) {
            if param.ty != arg.ty {
                matches = false;
                break;
            }
        }
        if matches {
            return Some(method);
        }
    }
    None
}

/// Type check a single function parameter.
pub(crate) fn type_parameter(
    ura: &UniversalReefAccessor,
    reef: ReefId,
    param: &FunctionParameter,
    source: SourceId,
) -> Parameter {
    match param {
        FunctionParameter::Named(named) => {
            let type_id = named
                .ty
                .as_ref()
                .map_or(STRING, |ty| resolve_type(ura, reef, source, ty));
            Parameter {
                location: Some(SourceLocation::new(source, named.segment.clone())),
                ty: type_id,
            }
        }
        FunctionParameter::Variadic(_) => todo!("Arrays are not supported yet"),
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

pub(crate) fn signature_to_string(
    name: &str,
    function: &FunctionType,
    ura: &UniversalReefAccessor,
) -> String {
    let mut buff = String::new();

    buff.push_str(name);
    buff.push('(');

    fn type_to_string(tpe: TypeRef, ura: &UniversalReefAccessor) -> String {
        let tpe = ura
            .get_types(tpe.reef)
            .and_then(|types| types.typing.get_type(tpe.type_id))
            .unwrap_or(&Type::Error);

        tpe.to_string()
    }

    if let Some((first, parameters)) = function.parameters.split_first() {
        buff.push_str(&type_to_string(first.ty, ura));
        for param in parameters {
            buff.push_str(", ");
            buff.push_str(&type_to_string(param.ty, ura));
        }
    }

    buff.push(')');
    if function.return_type.is_something() {
        buff.push_str(" -> ");
        buff.push_str(&type_to_string(function.return_type, ura));
    }

    buff
}
