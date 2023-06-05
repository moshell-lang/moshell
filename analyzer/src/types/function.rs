use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::types::ctx::TypeContext;
use crate::types::exploration::Exploration;
use crate::types::hir::{ExprKind, TypeId, TypedExpr};
use crate::types::ty::Parameter;
use crate::types::{ERROR, NOTHING, STRING};
use ast::function::{FunctionDeclaration, FunctionParameter};
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Return {
    pub(crate) ty: TypeId,
    pub(crate) segment: SourceSegment,
}

/// Gets the returned type of a function.
pub(crate) fn infer_return(
    func: &FunctionDeclaration,
    typed_func: &TypedExpr,
    diagnostics: &mut Vec<Diagnostic>,
    exploration: &mut Exploration,
) -> TypeId {
    let last = get_last_segment(typed_func);
    // If the last statement is a return, we don't need re-add it
    if exploration
        .returns
        .last()
        .map(|ret| ret.segment != last.segment.clone())
        .unwrap_or(true)
        && (last.ty.is_something()
            || !exploration.returns.is_empty() && func.return_type.as_ref().is_none())
    {
        exploration.returns.push(Return {
            ty: typed_func.ty,
            segment: last.segment.clone(),
        });
    }
    if let Some(return_type_annotation) = func.return_type.as_ref() {
        let type_annotation = exploration
            .ctx
            .resolve(return_type_annotation)
            .unwrap_or(ERROR);
        if type_annotation == ERROR {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::UnknownType,
                    exploration.ctx.source,
                    "Unknown type annotation",
                )
                .with_observation(Observation::new(return_type_annotation.segment())),
            );
        } else {
            for ret in &exploration.returns {
                if exploration.typing.unify(type_annotation, ret.ty).is_err() {
                    diagnostics.push(
                        Diagnostic::new(
                            DiagnosticID::TypeMismatch,
                            exploration.ctx.source,
                            "Type mismatch",
                        )
                        .with_observation(Observation::with_help(
                            ret.segment.clone(),
                            format!("Found `{}`", exploration.get_type(ret.ty).unwrap()),
                        ))
                        .with_observation(Observation::with_help(
                            return_type_annotation.segment(),
                            format!(
                                "Expected `{}` because of return type",
                                exploration.get_type(type_annotation).unwrap()
                            ),
                        )),
                    );
                }
            }
        }
        type_annotation
    } else if !matches!(func.body.as_ref(), Expr::Block(_)) {
        match exploration
            .typing
            .unify_many(exploration.returns.iter().map(|ret| ret.ty))
        {
            Ok(ty) if ty.is_nothing() => ty,
            Ok(ty) => {
                let segment = func.segment().start..func.body.segment().start;
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::TypeMismatch,
                        exploration.ctx.source,
                        "Return type inference is not supported yet",
                    )
                    .with_observation(Observation::with_help(
                        segment,
                        "No return type is specified",
                    ))
                    .with_tip("Add -> Float to the function declaration"),
                );
                ty
            }
            Err(_) => {
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::TypeMismatch,
                        exploration.ctx.source,
                        "Failed to infer return type",
                    )
                    .with_observation(Observation::with_help(
                        func.segment(),
                        "This function returns multiple types".to_string(),
                    ))
                    .with_tip("Try adding an explicit return type to the function"),
                );
                ERROR
            }
        }
    } else {
        let mut observations = Vec::new();
        for ret in &exploration.returns {
            observations.push(Observation::with_help(
                ret.segment.clone(),
                format!("Returning `{}`", exploration.get_type(ret.ty).unwrap()),
            ));
        }
        if !observations.is_empty() {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    exploration.ctx.source,
                    "Return type is not inferred for block functions",
                )
                .with_observations(observations)
                .with_tip("Try adding an explicit return type to the function"),
            );
        }
        NOTHING
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
