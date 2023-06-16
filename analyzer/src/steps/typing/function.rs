use crate::diagnostic::{Diagnostic, DiagnosticID, Observation, ObservationTag};
use crate::relations::{Relations, Symbol};
use crate::steps::typing::exploration::Exploration;
use crate::steps::typing::TypingState;
use crate::types::ctx::TypeContext;
use crate::types::hir::{ExprKind, TypeId, TypedExpr};
use crate::types::ty::{Parameter, Type};
use crate::types::{ERROR, NOTHING, STRING};
use ast::call::ProgrammaticCall;
use ast::function::{FunctionDeclaration, FunctionParameter};
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Return {
    pub(crate) ty: TypeId,
    pub(crate) segment: SourceSegment,
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
    state: TypingState,
) -> TypeId {
    let last = get_last_segment(typed_func);
    // If the last statement is a return, we don't need re-add it
    if exploration
        .returns
        .last()
        .map(|ret| ret.segment != last.segment)
        .unwrap_or(true)
        && (last.ty.is_something() || !exploration.returns.is_empty() && func.return_type.is_none())
    {
        exploration.returns.push(Return {
            ty: typed_func.ty,
            segment: last.segment.clone(),
        });
    }

    if let Some(return_type_annotation) = func.return_type.as_ref() {
        // An explicit return type is present, check it against all the return types.
        let type_annotation = exploration
            .ctx
            .resolve(return_type_annotation)
            .unwrap_or(ERROR);
        if type_annotation == ERROR {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::UnknownType,
                    state.source,
                    "Unknown type annotation",
                )
                .with_observation(
                    Observation::new(return_type_annotation.segment())
                        .with_help("Not found in scope"),
                ),
            );
        } else {
            for ret in &exploration.returns {
                if exploration.typing.unify(type_annotation, ret.ty).is_err() {
                    diagnostics.push(
                        Diagnostic::new(DiagnosticID::TypeMismatch, state.source, "Type mismatch")
                            .with_observation(Observation::new(ret.segment.clone()).with_help(
                                format!("Found `{}`", exploration.get_type(ret.ty).unwrap()),
                            ))
                            .with_observation(
                                Observation::new(return_type_annotation.segment()).with_help(
                                    format!(
                                        "Expected `{}` because of return type",
                                        exploration.get_type(type_annotation).unwrap()
                                    ),
                                ),
                            ),
                    );
                }
            }
        }
        type_annotation
    } else if !matches!(func.body.as_ref(), Expr::Block(_)) {
        // We may want to infer, or leave it as a empty return type
        match exploration
            .typing
            .unify_many(exploration.returns.iter().map(|ret| ret.ty))
        {
            Ok(ty) if ty.is_nothing() => ty,
            Ok(ty) => {
                let segment = func.segment().start..func.body.segment().start;
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::CannotInfer,
                        state.source,
                        "Return type inference is not supported yet",
                    )
                    .with_observation(
                        Observation::new(segment).with_help("No return type is specified"),
                    )
                    .with_help("Add -> Float to the function declaration"),
                );
                ty
            }
            Err(_) => {
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::CannotInfer,
                        state.source,
                        "Failed to infer return type",
                    )
                    .with_observation(
                        Observation::new(func.segment())
                            .with_help("This function returns multiple types"),
                    )
                    .with_help("Try adding an explicit return type to the function"),
                );
                ERROR
            }
        }
    } else {
        // Explain if there is any return that this function will not be inferred
        let mut observations = Vec::new();
        for ret in &exploration.returns {
            observations.push(Observation::new(ret.segment.clone()).with_help(format!(
                "Returning `{}`",
                exploration.get_type(ret.ty).unwrap()
            )));
        }
        if !observations.is_empty() {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::CannotInfer,
                    state.source,
                    "Return type is not inferred for block functions",
                )
                .with_observations(observations)
                .with_help("Try adding an explicit return type to the function"),
            );
        }
        NOTHING
    }
}

/// Checks the type of a call expression.
pub(super) fn type_call(
    call: &ProgrammaticCall,
    arguments: &[TypedExpr],
    symbol: Symbol,
    diagnostics: &mut Vec<Diagnostic>,
    exploration: &mut Exploration,
    relations: &Relations,
    state: TypingState,
) -> TypeId {
    let type_id = exploration
        .ctx
        .get(relations, state.source, symbol)
        .unwrap();
    match exploration.get_type(type_id).unwrap() {
        Type::Function(declaration) => {
            let declaration = *declaration;
            let entry = exploration.engine.get(declaration).unwrap();
            let parameters = &entry.parameters;
            let return_type = entry.return_type;
            if parameters.len() != arguments.len() {
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::TypeMismatch,
                        state.source,
                        format!(
                            "This function takes {} {} but {} were supplied",
                            parameters.len(),
                            pluralize(parameters.len(), "argument", "arguments"),
                            arguments.len()
                        ),
                    )
                    .with_observation(
                        Observation::new(call.segment.clone()).with_help("Function is called here"),
                    ),
                );
                ERROR
            } else {
                for (param, arg) in parameters.iter().zip(arguments.iter()) {
                    if exploration.typing.unify(param.ty, arg.ty).is_err() {
                        diagnostics.push(
                            Diagnostic::new(
                                DiagnosticID::TypeMismatch,
                                state.source,
                                "Type mismatch",
                            )
                            .with_observation(
                                Observation::new(arg.segment.clone())
                                    .with_help(format!(
                                        "Expected `{}`, found `{}`",
                                        exploration.get_type(param.ty).unwrap(),
                                        exploration.get_type(arg.ty).unwrap()
                                    ))
                                    .with_tag(ObservationTag::InFault),
                            )
                            .with_observation(
                                Observation::new(param.segment.clone())
                                    .within(declaration)
                                    .with_help("Parameter is declared here")
                                    .with_tag(ObservationTag::Declaration),
                            ),
                        );
                    }
                }
                return_type
            }
        }
        ty => {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    state.source,
                    "Cannot invoke non function type",
                )
                .with_observation(
                    Observation::new(call.segment())
                        .with_help(format!("Call expression requires function, found `{ty}`")),
                ),
            );
            ERROR
        }
    }
}

/// Type check a single function parameter.
pub(crate) fn type_parameter(ctx: &TypeContext, param: &FunctionParameter) -> Parameter {
    match param {
        FunctionParameter::Named(named) => {
            let type_id = named
                .ty
                .as_ref()
                .map(|ty| ctx.resolve(ty).unwrap_or(ERROR))
                .unwrap_or(STRING);
            Parameter {
                segment: named.segment.clone(),
                ty: type_id,
            }
        }
        FunctionParameter::Variadic(_) => todo!("Arrays are not supported yet"),
    }
}

fn get_last_segment(expr: &TypedExpr) -> &TypedExpr {
    match &expr.kind {
        ExprKind::Block(expressions) => expressions.last().map(get_last_segment).unwrap_or(expr),
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
