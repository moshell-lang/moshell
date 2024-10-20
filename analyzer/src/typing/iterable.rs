use crate::hir::{ExprKind, ForLoop, FunctionCall, Module, RangeFor, TypedExpr};
use crate::typing::registry::{INCLUSIVE_RANGE_SCHEMA, RANGE_SCHEMA, STRING_SCHEMA, VEC_SCHEMA};
use crate::typing::user::{UserType, INT_TYPE, STRING_TYPE, UNIT_TYPE};
use crate::typing::variable::{Var, VariableTable};
use crate::typing::{ascribe_type, Context, TypeChecker, TypeError, TypeErrorKind, TypeHint};
use crate::{hir, SourceLocation};
use ast::control_flow::{For, ForKind};
use ast::range::NumericRange;
use ast::value::LiteralValue;
use context::source::SourceSegmentHolder;
use std::ffi::OsStr;

pub(super) fn ascribe_for(
    it: &For,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    table.enter_scope();
    let typed_expr = match it.kind.as_ref() {
        ForKind::Range(range) => {
            let iterable = ascribe_type(&range.iterable, table, checker, storage, ctx, errors);
            let receiver_type = match checker.types[iterable.ty] {
                UserType::Parametrized { schema, ref params } if schema == VEC_SCHEMA => params[0],
                UserType::Parametrized { schema, .. } if schema == STRING_SCHEMA => STRING_TYPE,
                UserType::Parametrized { schema, .. }
                    if schema == RANGE_SCHEMA || schema == INCLUSIVE_RANGE_SCHEMA =>
                {
                    INT_TYPE
                }
                _ => {
                    errors.push(TypeError::new(
                        TypeErrorKind::TraitNotImplemented {
                            trait_name: "Iterator".to_owned(),
                            type_name: checker.display(iterable.ty),
                        },
                        SourceLocation::new(table.path().to_owned(), iterable.span.clone()),
                    ));
                    iterable.ty
                }
            };
            let Var::Local(receiver) = table.insert_variable(
                range.receiver.to_string(),
                receiver_type,
                range.iterable.segment(),
                false,
            ) else {
                panic!("Expected a local variable");
            };
            let ctx = ctx.with_hint(TypeHint::Unused).in_loop();
            let body = ascribe_type(&it.body, table, checker, storage, ctx, errors);
            TypedExpr {
                kind: ExprKind::ForLoop(ForLoop {
                    kind: Box::new(hir::ForKind::Range(RangeFor {
                        receiver,
                        receiver_type,
                        iterable,
                    })),
                    body: Box::new(body),
                }),
                ty: UNIT_TYPE,
                span: it.segment.clone(),
            }
        }
        ForKind::Conditional(conditional) => {
            let initializer = ascribe_type(
                &conditional.initializer,
                table,
                checker,
                storage,
                ctx,
                errors,
            );
            let condition =
                ascribe_type(&conditional.condition, table, checker, storage, ctx, errors);
            let increment =
                ascribe_type(&conditional.increment, table, checker, storage, ctx, errors);
            let ctx = ctx.with_hint(TypeHint::Unused).in_loop();
            let body = ascribe_type(&it.body, table, checker, storage, ctx, errors);
            TypedExpr {
                kind: ExprKind::ForLoop(ForLoop {
                    kind: Box::new(hir::ForKind::Conditional(hir::ConditionalFor {
                        initializer,
                        condition,
                        increment,
                    })),
                    body: Box::new(body),
                }),
                ty: UNIT_TYPE,
                span: it.segment.clone(),
            }
        }
    };
    table.exit_scope();
    typed_expr
}

pub(super) fn ascribe_range(
    range: &NumericRange,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let ctx = ctx.with_hint(TypeHint::Required(INT_TYPE));
    let start = ascribe_type(&range.start, table, checker, storage, ctx, errors);
    let end = ascribe_type(&range.end, table, checker, storage, ctx, errors);
    let step = range
        .step
        .as_ref()
        .map(|step| ascribe_type(step, table, checker, storage, ctx, errors))
        .unwrap_or_else(|| TypedExpr {
            kind: ExprKind::Literal(LiteralValue::Int(1)),
            ty: INT_TYPE,
            span: range.segment(),
        });

    let args = [&start, &end, &step];
    for arg in &args {
        if !checker.types.are_same(arg.ty, INT_TYPE) {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "Int".to_owned(),
                    expected_due_to: None,
                    actual: checker.display(arg.ty),
                },
                SourceLocation::new(table.path().to_owned(), arg.span.clone()),
            ));
        }
    }
    let range_ty = ctx
        .modules
        .foreign
        .get(OsStr::new("std"))
        .and_then(|module| module.exports.iter().find(|export| export.name == "Range"))
        .map(|range| range.ty)
        .expect("Range type not found");
    let UserType::Parametrized { schema, params: _ } = checker.types[range_ty] else {
        panic!("Expected a parametrized type");
    };
    let constructor_id = checker.registry[schema]
        .get_exact_method(
            &checker.types,
            &checker.registry,
            "<init>",
            &[INT_TYPE, INT_TYPE, INT_TYPE],
            range_ty,
        )
        .expect("Range type does not have a constructor");
    TypedExpr {
        kind: ExprKind::FunctionCall(FunctionCall {
            function_id: constructor_id,
            arguments: vec![start, end, step],
        }),
        ty: range_ty,
        span: range.segment(),
    }
}

#[cfg(test)]
mod tests {
    use crate::typing::tests::type_check;
    use crate::typing::{TypeError, TypeErrorKind};
    use crate::SourceLocation;
    use std::path::PathBuf;

    #[test]
    fn not_iterable() {
        let errors = type_check("for x in 42 { }");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TraitNotImplemented {
                    trait_name: "Iterator".to_owned(),
                    type_name: "Int".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 9..11),
            )]
        );
    }
}
