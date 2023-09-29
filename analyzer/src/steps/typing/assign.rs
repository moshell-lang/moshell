use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::steps::typing::exploration::{Exploration, Links};
use crate::steps::typing::function::{
    find_exact_method, find_operand_implementation, list_operator_defined_for, BinaryMethodMatch,
};
use crate::steps::typing::{ascribe_types, TypingState};
use crate::types::hir::{ExprKind, MethodCall, TypedExpr};
use ast::range::Subscript;
use ast::variable::Assign;
use context::source::SourceSegmentHolder;

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
        .get_methods(target.ty, "[]")
        .map(|methods| methods.as_slice())
        .unwrap_or(&[]);
    let method = find_operand_implementation(exploration, target.ty.reef, methods, target, index);
    match method {
        Ok(method) => Ok(method),
        Err(target) => {
            diagnostics.push(if !methods.is_empty() {
                Diagnostic::new(
                    DiagnosticID::UnknownMethod,
                    format!(
                        "Cannot index into a value of type `{}`",
                        exploration.get_type(target_ty)
                    ),
                )
                .with_observation(Observation::here(
                    links.source,
                    exploration.externals.current,
                    sub.index.segment(),
                    format!(
                        "`{}` indices are of type {}",
                        exploration.get_type(target_ty),
                        list_operator_defined_for(exploration, methods),
                    ),
                ))
            } else {
                Diagnostic::new(
                    DiagnosticID::UnknownMethod,
                    format!(
                        "The type `{}` cannot be indexed by `{}`",
                        exploration.get_type(target_ty),
                        exploration.get_type(index_ty)
                    ),
                )
                .with_observation(Observation::here(
                    links.source,
                    exploration.externals.current,
                    sub.index.segment(),
                    format!(
                        "Indexing with `{}` is invalid",
                        exploration.get_type(index_ty)
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
    rhs: TypedExpr,
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
        state.with_local_type(),
    )
    else {
        return rhs.poison();
    };

    let rhs_segment = rhs.segment();
    let rhs_ty = rhs.ty;
    let methods = exploration
        .get_methods(target.ty, "[]")
        .map(|methods| methods.as_slice())
        .unwrap_or(&[]);
    let mut args = vec![index, rhs];
    let method = find_exact_method(exploration, target.ty, methods, args.as_slice());
    if let Some(method) = method {
        let return_type = exploration.concretize(method.return_type, target.ty).id;
        return TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(target),
                arguments: args,
                definition: method.definition,
            }),
            ty: return_type,
            segment: assign.segment(),
        };
    }
    diagnostics.push(
        Diagnostic::new(
            DiagnosticID::TypeMismatch,
            format!(
                "Invalid assignment to `{}`",
                exploration.get_type(target.ty)
            ),
        )
        .with_observation(Observation::here(
            links.source,
            exploration.externals.current,
            rhs_segment,
            format!("Found `{}`", exploration.get_type(rhs_ty)),
        ))
        .with_observation(Observation::context(
            links.source,
            exploration.externals.current,
            sub.segment(),
            "Expected due to the type of this binding",
        )),
    );
    args.pop().unwrap().poison()
}
