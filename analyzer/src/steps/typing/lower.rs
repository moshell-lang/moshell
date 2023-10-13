use context::source::SourceSegmentHolder;

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::relations::SourceId;
use crate::steps::typing::bounds::TypesBounds;
use crate::steps::typing::exploration::Exploration;
use crate::steps::typing::view::TypeView;
use crate::types::hir::{ExprKind, MethodCall, TypedExpr};
use crate::types::ty::TypeRef;
use crate::types::{BOOL, FLOAT, STRING};

pub fn get_converter(ty: TypeRef) -> Option<&'static str> {
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
    source: SourceId,
) -> TypedExpr {
    call_convert_on(
        expr,
        STRING,
        exploration,
        |ty| format!("Cannot stringify type `{ty}`",),
        diagnostics,
        &TypesBounds::inactive(),
        source,
    )
}

/// Generates a conversion method call if needed.
///
/// This function must be called only if a conversion has been accepted by the type engine,
/// use the upper level function [`crate::steps::typing::coercion::convert_expression`] to
/// do the proper checks.
pub(super) fn call_convert_on(
    expr: TypedExpr,
    into: TypeRef,
    exploration: &Exploration,
    message: impl FnOnce(TypeView) -> String,
    diagnostics: &mut Vec<Diagnostic>,
    bounds: &TypesBounds,
    source: SourceId,
) -> TypedExpr {
    // If the expression is already of the needed type, we don't need to do anything.
    // The `Nothing` type can be converted to anything, so we also return early.
    if exploration.is_compatible(into, expr.ty) {
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
                        exploration.new_type_view(into, bounds)
                    ),
                )
                .with_observation((source, exploration.externals.current, expr.segment()).into()),
            );
            return expr;
        }
    };

    // Else, we try to find the expected conversion method on the expression's type
    if let Some((method, method_id)) = exploration.get_method_exact(expr.ty, method_name, &[], into)
    {
        let segment = expr.segment.clone();
        return TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(expr),
                arguments: vec![],
                function_id: method_id,
            }),
            ty: method.return_type,
            segment,
        };
    }

    diagnostics.push(
        Diagnostic::new(
            DiagnosticID::TypeMismatch,
            message(exploration.new_type_view(expr.ty, bounds)),
        )
        .with_observation(Observation::here(
            source,
            exploration.externals.current,
            expr.segment(),
            format!(
                "No method `{method_name}` on type `{}`",
                exploration.new_type_view(expr.ty, bounds)
            ),
        )),
    );
    expr
}
