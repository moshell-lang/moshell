use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::steps::typing::exploration::Exploration;
use crate::steps::typing::TypingState;
use crate::types::hir::{ExprKind, TypedExpr};
use crate::types::STRING;
use context::source::SourceSegmentHolder;

/// Try to convert an expression into a string.
pub(super) fn convert_into_string(
    expr: TypedExpr,
    exploration: &Exploration,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    // If the expression is already a string, we don't need to do anything.
    if expr.ty.is_err() || expr.ty == STRING {
        return expr;
    }

    // Else, we try to find a `to_string` method on the type.
    if let Some(to_string) = exploration
        .engine
        .get_method_exact(expr.ty, "to_string", &[], STRING)
    {
        let segment = expr.segment.clone();
        TypedExpr {
            kind: ExprKind::MethodCall {
                callee: Box::new(expr),
                arguments: vec![],
                definition: to_string.definition,
            },
            ty: to_string.return_type,
            segment,
        }
    } else {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::UnknownMethod,
                state.source,
                format!(
                    "Cannot stringify type `{}`",
                    exploration.get_type(expr.ty).unwrap()
                ),
            )
            .with_observation(Observation::with_help(
                expr.segment(),
                format!(
                    "No method `to_string` on type `{}`",
                    exploration.get_type(expr.ty).unwrap()
                ),
            )),
        );
        expr
    }
}
