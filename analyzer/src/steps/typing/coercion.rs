use crate::diagnostic::{Diagnostic, DiagnosticID, Observation, ObservationTag};
use crate::steps::typing::exploration::{diagnose_unknown_type, Exploration};
use crate::steps::typing::lower::call_convert_on;
use crate::steps::typing::TypingState;
use crate::types::engine::TypedEngine;
use crate::types::hir::{TypeId, TypedExpr};
use crate::types::{Typing, BOOL, ERROR};
use context::source::SourceSegmentHolder;

/// Ensures that the type annotation accepts the given value.
///
/// A type annotation might generate a conversion function call, which is returned.
pub(super) fn check_type_annotation(
    exploration: &mut Exploration,
    type_annotation: &ast::r#type::Type,
    value: TypedExpr,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let expected_type = exploration.ctx.resolve(type_annotation).unwrap_or(ERROR);
    if expected_type.is_err() {
        diagnostics.push(diagnose_unknown_type(
            state.source,
            type_annotation.segment(),
        ));
        return value;
    }
    if value.ty.is_err() {
        return value;
    }

    convert_expression(
        value,
        expected_type,
        &mut exploration.typing,
        &exploration.engine,
        state,
        diagnostics,
    )
    .unwrap_or_else(|value| {
        diagnostics.push(
            Diagnostic::new(DiagnosticID::TypeMismatch, state.source, "Type mismatch")
                .with_observation(
                    Observation::new(type_annotation.segment())
                        .with_help(format!(
                            "Expected `{}`",
                            exploration.get_type(expected_type).unwrap()
                        ))
                        .with_tag(ObservationTag::Expected),
                )
                .with_observation(
                    Observation::new(value.segment())
                        .with_help(format!(
                            "Found `{}`",
                            exploration.get_type(value.ty).unwrap()
                        ))
                        .with_tag(ObservationTag::InFault),
                ),
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
    assign_to: TypeId,
    typing: &mut Typing,
    engine: &TypedEngine,
    state: TypingState,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<TypedExpr, TypedExpr> {
    match typing.convert_description(assign_to, rvalue.ty) {
        Ok(ty) => Ok(call_convert_on(
            rvalue,
            typing,
            engine,
            ty,
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
    exploration: &mut Exploration,
    state: TypingState,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedExpr {
    match convert_expression(
        condition,
        BOOL,
        &mut exploration.typing,
        &exploration.engine,
        state,
        diagnostics,
    ) {
        Ok(condition) => condition,
        Err(condition) => {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    state.source,
                    "Condition must be a boolean",
                )
                .with_observation(Observation::new(condition.segment()).with_help(
                    format!(
                        "Type `{}` cannot be used as a condition",
                        exploration.get_type(condition.ty).unwrap()
                    ),
                )),
            );
            condition
        }
    }
}
