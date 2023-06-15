use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::steps::typing::exploration::Exploration;
use crate::steps::typing::TypingState;
use crate::types::engine::TypedEngine;
use crate::types::hir::{ExprKind, TypeId, TypedExpr};
use crate::types::ty::Type;
use crate::types::{Typing, BOOL, FLOAT, STRING};
use context::source::SourceSegmentHolder;

pub fn get_converter(ty: TypeId) -> Option<&'static str> {
    Some(match ty {
        BOOL => "to_bool",
        FLOAT => "to_float",
        STRING => "to_string",
        _ => return None,
    })
}

/// Try to convert an expression into a string.
pub(super) fn convert_into_string(
    expr: TypedExpr,
    exploration: &Exploration,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    call_convert_on(
        expr,
        &exploration.typing,
        &exploration.engine,
        STRING,
        |ty| format!("Cannot stringify type `{}`", ty),
        diagnostics,
        state,
    )
}

pub(super) fn call_convert_on(
    expr: TypedExpr,
    typing: &Typing,
    engine: &TypedEngine,
    into: TypeId,
    message: impl FnOnce(&Type) -> String,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    // If the expression is already of the needed type, we don't need to do anything.
    if expr.ty.is_err() || expr.ty == into {
        return expr;
    }

    let method_name = match get_converter(into) {
        Some(method_name) => method_name,
        None => {
            panic!(
                "No converted defined for type `{}`",
                typing.get_type(into).unwrap()
            );
        }
    };

    // Else, we try to find a `to_string` method on the type.
    if let Some(method) = engine.get_method_exact(expr.ty, method_name, &[], into) {
        let segment = expr.segment.clone();
        TypedExpr {
            kind: ExprKind::MethodCall {
                callee: Box::new(expr),
                arguments: vec![],
                definition: method.definition,
            },
            ty: method.return_type,
            segment,
        }
    } else {
        let ty = typing.get_type(expr.ty).unwrap();
        diagnostics.push(
            Diagnostic::new(DiagnosticID::TypeMismatch, state.source, message(ty))
                .with_observation(Observation::with_help(
                    expr.segment(),
                    format!("No method `{}` on type `{}`", method_name, ty),
                )),
        );
        expr
    }
}
