use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::steps::typing::bounds::TypesBounds;
use crate::steps::typing::coercion::{convert_expression, is_compatible};
use crate::steps::typing::exploration::{Exploration, Links};
use crate::steps::typing::function::{
    find_operand_implementation, list_operator_defined_for, BinaryMethodMatch,
};
use crate::steps::typing::{ascribe_types, ExpressionValue, TypingState};
use crate::types::hir::{ExprKind, MethodCall, TypedExpr};
use crate::types::ty::Parameter;
use crate::types::UNIT;
use ast::operation::{BinaryOperation, BinaryOperator};
use ast::range::Subscript;
use ast::variable::{Assign, AssignOperator};
use ast::Expr;
use context::source::SourceSegmentHolder;

/// Creates the right hand side of an assignment.
///
/// The state should contain the [`ExpressionValue::Expected`] value of the left hand side.
pub(super) fn ascribe_assign_rhs(
    assign: &Assign,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    match assign.operator {
        AssignOperator::Assign => {
            ascribe_types(exploration, links, diagnostics, &assign.value, state)
        }
        operator => {
            let binary = Expr::Binary(BinaryOperation {
                left: assign.left.clone(),
                op: BinaryOperator::try_from(operator).expect("Invalid assign operator"),
                right: assign.value.clone(),
            });
            ascribe_types(
                exploration,
                links,
                diagnostics,
                &binary,
                state.with_local_value(ExpressionValue::Unspecified),
            )
        }
    }
}

pub(super) fn create_subscript(
    sub: &Subscript,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> Result<BinaryMethodMatch, TypedExpr> {
    let target = ascribe_types(exploration, links, diagnostics, &sub.target, state);
    let index = ascribe_types(exploration, links, diagnostics, &sub.index, state);
    if index.ty.is_err() || target.ty.is_err() {
        return Err(target);
    }

    let index_ty = index.ty;
    let target_ty = target.ty;
    let methods = exploration
        .get_methods(target_ty, "[]")
        .map(|methods| methods.as_slice())
        .unwrap_or(&[]);

    let target_ty_base_reef = exploration.get_base_type(target_ty).reef;
    let method =
        find_operand_implementation(exploration, target_ty_base_reef, methods, target, index);
    match method {
        Ok(method) => Ok(method),
        Err(target) => {
            diagnostics.push(if !methods.is_empty() {
                let methods: Vec<_> = methods
                    .iter()
                    .flat_map(|method_id| exploration.get_function(target_ty_base_reef, *method_id))
                    .collect();

                Diagnostic::new(
                    DiagnosticID::UnknownMethod,
                    format!(
                        "Cannot index into a value of type `{}`",
                        exploration.new_type_view(target_ty, &TypesBounds::inactive())
                    ),
                )
                .with_observation(Observation::here(
                    links.source,
                    exploration.externals.current,
                    sub.index.segment(),
                    format!(
                        "`{}` indices are of type {}",
                        exploration.new_type_view(target_ty, &TypesBounds::inactive()),
                        list_operator_defined_for(exploration, &methods, &TypesBounds::inactive()),
                    ),
                ))
            } else {
                Diagnostic::new(
                    DiagnosticID::UnknownMethod,
                    format!(
                        "The type `{}` cannot be indexed by `{}`",
                        exploration.new_type_view(target_ty, &TypesBounds::inactive()),
                        exploration.new_type_view(index_ty, &TypesBounds::inactive())
                    ),
                )
                .with_observation(Observation::here(
                    links.source,
                    exploration.externals.current,
                    sub.index.segment(),
                    format!(
                        "Indexing with `{}` is invalid",
                        exploration.new_type_view(index_ty, &TypesBounds::inactive())
                    ),
                ))
            });
            Err(target)
        }
    }
}

pub(super) fn ascribe_assign_subscript(
    assign: &Assign,
    sub: &Subscript,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    // Require first that normal subscripting is available
    let Ok(BinaryMethodMatch {
        left: target,
        right: index,
        ..
    }) = create_subscript(
        sub,
        exploration,
        links,
        diagnostics,
        state.with_local_value(ExpressionValue::Unspecified),
    )
    else {
        return TypedExpr::error(assign.segment());
    };

    let target_type_reef = exploration.get_base_type(target.ty).reef;
    let Some((function_id, value_ty)) = exploration
        .get_methods(target.ty, "[]")
        .map(|methods| methods.as_slice())
        .unwrap_or(&[])
        .iter()
        .find_map(|method_id| {
            // Look for the method without worrying about potential overloads for the second parameter
            let method = exploration
                .get_function(target_type_reef, *method_id)
                .unwrap();
            if let [Parameter { ty: index_ty, .. }, value] = method.parameters.as_slice() {
                if !is_compatible(exploration, *index_ty, index.ty) || method.return_type != UNIT {
                    return None;
                }
                Some((*method_id, exploration.concretize(value.ty, target.ty)))
            } else {
                None
            }
        })
    else {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::TypeMismatch,
                format!(
                    "Type `{}` is indexable but is not assignable",
                    exploration.new_type_view(target.ty, &TypesBounds::inactive())
                ),
            )
            .with_observation(Observation::here(
                links.source,
                exploration.externals.current,
                sub.segment(),
                format!(
                    "Indexing with `{}` does not allow assignment",
                    exploration.new_type_view(index.ty, &TypesBounds::inactive())
                ),
            )),
        );
        return TypedExpr::error(assign.segment());
    };

    let rhs_state = state.with_local_value(ExpressionValue::Expected(value_ty));
    let rhs = ascribe_assign_rhs(assign, exploration, links, diagnostics, rhs_state);
    let rhs_segment = rhs.segment();
    let rhs_ty = rhs.ty;

    if let Ok(converted) = convert_expression(
        rhs,
        value_ty,
        &mut TypesBounds::inactive(),
        exploration,
        links.source,
        diagnostics,
    ) {
        return TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(target),
                arguments: vec![index, converted],
                function_id,
            }),
            ty: UNIT,
            segment: assign.segment(),
        };
    }
    diagnostics.push(
        Diagnostic::new(
            DiagnosticID::TypeMismatch,
            format!(
                "Invalid assignment to `{}`",
                exploration.new_type_view(value_ty, &TypesBounds::inactive())
            ),
        )
        .with_observation(Observation::here(
            links.source,
            exploration.externals.current,
            rhs_segment,
            format!(
                "Found `{}`",
                exploration.new_type_view(rhs_ty, &TypesBounds::inactive())
            ),
        ))
        .with_observation(Observation::context(
            links.source,
            exploration.externals.current,
            sub.segment(),
            "Expected due to the type of this binding",
        )),
    );
    TypedExpr::error(assign.segment())
}
