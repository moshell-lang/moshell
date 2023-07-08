use context::source::SourceSegmentHolder;

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::steps::typing::exploration::Exploration;
use crate::steps::typing::TypingState;
use crate::types::engine::TypedEngine;
use crate::types::hir::{ExprKind, MethodCall, TypeId, TypedExpr};
use crate::types::ty::Type;
use crate::types::{Typing, BOOL, FLOAT, STRING};

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
        |ty| format!("Cannot stringify type `{ty}`"),
        diagnostics,
        state,
    )
}

/// Generates a conversion method call if needed.
///
/// This function must be called only if a conversion has been accepted by the type engine,
/// use the upper level function [`crate::steps::typing::coercion::convert_expression`] to
/// do the proper checks.
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
    // The `Nothing` type can be converted to anything, so we also return early.
    if expr.ty.is_err() || into.is_err() || expr.ty == into || expr.ty.is_nothing() {
        return expr;
    }

    let method_name = match get_converter(into) {
        Some(method_name) => method_name,
        None => {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::UnknownMethod,
                    format!(
                        "No conversion method defined for type `{}`",
                        typing.get_type(into).unwrap()
                    ),
                )
                .with_observation(Observation::underline(state.source, expr.segment())),
            );
            return expr;
        }
    };

    // Else, we try to find the expected conversion method on the expression's type
    if let Some(method) = engine.get_method_exact(expr.ty, method_name, &[], into) {
        let segment = expr.segment.clone();
        return TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(expr),
                arguments: vec![],
                definition: method.definition,
            }),
            ty: method.return_type,
            segment,
        };
    }

    let ty = typing.get_type(expr.ty).unwrap();
    diagnostics.push(
        Diagnostic::new(DiagnosticID::TypeMismatch, message(ty)).with_observation(
            Observation::here(
                state.source,
                expr.segment(),
                format!("No method `{method_name}` on type `{ty}`"),
            ),
        ),
    );
    expr
}
