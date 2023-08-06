use context::source::SourceSegmentHolder;

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::steps::typing::exploration::UniversalReefAccessor;
use crate::steps::typing::lower::call_convert_on;
use crate::steps::typing::TypingState;
use crate::types::hir::TypedExpr;
use crate::types::ty::TypeRef;
use crate::types::{convert_description, get_type, resolve_type, BOOL};

/// Ensures that the type annotation accepts the given value.
///
/// A type annotation might generate a conversion function call, which is returned.
pub(super) fn check_type_annotation(
    reefs: &UniversalReefAccessor,
    type_annotation: &ast::r#type::Type,
    value: TypedExpr,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    if value.ty.is_err() {
        return value;
    }

    let expected_type = resolve_type(reefs, state.reef, state.source, type_annotation);

    convert_expression(value, expected_type, state, reefs, diagnostics).unwrap_or_else(|value| {
        diagnostics.push(
            Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch")
                .with_observation(Observation::here(
                    state.source,
                    type_annotation.segment(),
                    format!("Expected `{}`", get_type(expected_type, reefs).unwrap()),
                ))
                .with_observation(Observation::here(
                    state.source,
                    value.segment(),
                    format!("Found `{}`", get_type(value.ty, reefs).unwrap()),
                )),
        );
        value
    })
}

/// Tries to convert an expression to the given assignation type.
///
/// If unified, the expression is converted using the appropriate method.
/// If the conversion is incorrect, the input expression is returned,
/// in order to encourage the caller to report a specific error.
///
/// Most of the times, it will not generate any diagnostic, since diagnostics
/// would only be generated if an implicit conversion is incorrect (i.e. if
/// it is registered but if the appropriate method is not found).
pub(super) fn convert_expression(
    rvalue: TypedExpr,
    assign_to: TypeRef,
    state: TypingState,
    ura: &UniversalReefAccessor,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<TypedExpr, TypedExpr> {
    match convert_description(ura, assign_to, rvalue.ty) {
        Ok(ty) => Ok(call_convert_on(
            rvalue,
            ty,
            ura,
            |ty| format!("Cannot convert type `{ty}`"),
            diagnostics,
            state,
        )),
        Err(_) => Err(rvalue),
    }
}

/// Ensures that the expression is a boolean.
///
/// If not, a diagnostic is generated and the expression is returned.
/// Otherwise, the converted expression is returned.
pub(super) fn coerce_condition(
    condition: TypedExpr,
    ura: &UniversalReefAccessor,
    state: TypingState,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedExpr {
    match convert_expression(condition, BOOL, state, ura, diagnostics) {
        Ok(condition) => condition,
        Err(condition) => {
            diagnostics.push(
                Diagnostic::new(DiagnosticID::TypeMismatch, "Condition must be a boolean")
                    .with_observation(Observation::here(
                        state.source,
                        condition.segment(),
                        format!(
                            "Type `{}` cannot be used as a condition",
                            get_type(condition.ty, ura).unwrap()
                        ),
                    )),
            );
            condition
        }
    }
}
