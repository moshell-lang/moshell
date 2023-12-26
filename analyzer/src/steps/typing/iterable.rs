use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::reef::ReefId;
use crate::relations::SymbolRef;
use crate::steps::typing::bounds::TypesBounds;
use crate::steps::typing::exploration::{Exploration, Links};
use crate::steps::typing::{ascribe_types, ExpressionValue, TypingState};
use crate::types::builtin::STRING_STRUCT;
use crate::types::ctx::TypedVariable;
use crate::types::engine::StructureId;
use crate::types::hir::{ConditionalFor, ExprKind, ForLoop, RangeFor, TypedExpr};
use crate::types::ty::Type;
use crate::types::{hir, ERROR, GENERIC_VECTOR, INT, UNIT};
use ast::control_flow::{For, ForKind};
use context::source::SourceSegmentHolder;

pub(super) fn ascribe_for(
    it: &For,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    match it.kind.as_ref() {
        ForKind::Range(range) => {
            let iterable = ascribe_types(exploration, links, diagnostics, &range.iterable, state);
            let id = links.env().get_raw_symbol(range.segment.clone()).unwrap();
            let SymbolRef::Local(receiver_id) = id else {
                unreachable!()
            };
            exploration
                .ctx
                .set_local(links.source, receiver_id, TypedVariable::immutable(ERROR));
            let iterable_type = exploration.get_type(iterable.ty).unwrap();
            match iterable_type {
                Type::Instantiated(vec, params) if *vec == GENERIC_VECTOR => {
                    let param = params[0];
                    exploration.ctx.set_local(
                        links.source,
                        receiver_id,
                        TypedVariable::immutable(param),
                    );
                }
                Type::Structure(_, string) if *string == STRING_STRUCT => {
                    exploration.ctx.set_local(
                        links.source,
                        receiver_id,
                        TypedVariable::immutable(iterable.ty),
                    );
                }
                Type::Structure(_, StructureId(0 | 1)) if iterable.ty.reef == ReefId(1) => {
                    exploration.ctx.set_local(
                        links.source,
                        receiver_id,
                        TypedVariable::immutable(INT),
                    );
                }
                _ => {
                    if iterable.ty.is_ok() {
                        diagnose_not_iterable(exploration, links, &iterable, diagnostics);
                    }
                }
            }
            let body = ascribe_types(
                exploration,
                links,
                diagnostics,
                &it.body,
                state
                    .with_in_loop()
                    .with_local_value(ExpressionValue::Unused),
            );
            TypedExpr {
                kind: ExprKind::ForLoop(ForLoop {
                    kind: Box::new(hir::ForKind::Range(RangeFor {
                        receiver: receiver_id,
                        receiver_type: exploration
                            .ctx
                            .get_local(links.source, receiver_id)
                            .unwrap()
                            .type_ref,
                        iterable,
                    })),
                    body: Box::new(body),
                }),
                ty: UNIT,
                segment: it.segment.clone(),
            }
        }
        ForKind::Conditional(conditional) => {
            let initializer = ascribe_types(
                exploration,
                links,
                diagnostics,
                &conditional.initializer,
                state,
            );
            let condition = ascribe_types(
                exploration,
                links,
                diagnostics,
                &conditional.condition,
                state,
            );
            let increment = ascribe_types(
                exploration,
                links,
                diagnostics,
                &conditional.increment,
                state,
            );
            let body = ascribe_types(
                exploration,
                links,
                diagnostics,
                &it.body,
                state
                    .with_in_loop()
                    .with_local_value(ExpressionValue::Unused),
            );
            TypedExpr {
                kind: ExprKind::ForLoop(ForLoop {
                    kind: Box::new(hir::ForKind::Conditional(ConditionalFor {
                        initializer,
                        condition,
                        increment,
                    })),
                    body: Box::new(body),
                }),
                ty: UNIT,
                segment: it.segment.clone(),
            }
        }
    }
}

fn diagnose_not_iterable(
    exploration: &Exploration,
    links: Links,
    iterable: &TypedExpr,
    diagnostics: &mut Vec<Diagnostic>,
) {
    diagnostics.push(
        Diagnostic::new(DiagnosticID::TypeMismatch, "Expected iterable type").with_observation(
            Observation::here(
                links.source,
                exploration.externals.current,
                iterable.segment(),
                format!(
                    "Found `{}`",
                    exploration.new_type_view(iterable.ty, &TypesBounds::inactive())
                ),
            ),
        ),
    );
}

#[cfg(test)]
mod tests {
    use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
    use crate::reef::ReefId;
    use crate::relations::SourceId;
    use crate::steps::typing::tests::extract_type;
    use crate::types::{STRING, UNIT};
    use context::source::Source;
    use context::str_find::find_in;
    use pretty_assertions::assert_eq;

    #[test]
    fn verify_body_when_error() {
        let content = "for i in 11 - {} { i = 9 }";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![
                Diagnostic::new(DiagnosticID::UnknownMethod, "Undefined operator",)
                    .with_observation(Observation::here(
                        SourceId(0),
                        ReefId(1),
                        find_in(content, "11 - {}"),
                        "No operator `sub` between type `Int` and `Unit`",
                    )),
                Diagnostic::new(
                    DiagnosticID::CannotReassign,
                    "Cannot assign twice to immutable variable `i`",
                )
                .with_observation(Observation::here(
                    SourceId(0),
                    ReefId(1),
                    find_in(content, "i = 9"),
                    "Assignment happens here",
                ))
            ])
        );
    }

    #[test]
    fn iterate_glob() {
        let source = Source::unknown("for f in p'*'.spread() { echo $f }");
        let res = extract_type(source);
        assert_eq!(res, Ok(UNIT));
    }

    #[test]
    fn iterate_string() {
        let source = Source::unknown("var last = ''; for c in 'hello' { last = $c }; $last");
        let res = extract_type(source);
        assert_eq!(res, Ok(STRING));
    }

    #[test]
    fn iterate_condition() {
        let source = Source::unknown("for ((var x = 0; $x < 0; $x += 0)) { echo $x }");
        let res = extract_type(source);
        assert_eq!(res, Ok(UNIT));
    }
}
