use crate::hir::{ExprKind, Loop, Module, TypedExpr};
use crate::typing::lower::coerce_condition;
use crate::typing::user::{BOOL_TYPE, NOTHING_TYPE, UNIT_TYPE};
use crate::typing::variable::VariableTable;
use crate::typing::{ascribe_type, Context, TypeChecker, TypeError, TypeErrorKind, TypeHint};
use crate::SourceLocation;
use ast::control_flow::While;
use context::source::{SourceSegment, SourceSegmentHolder};

pub(super) fn ascribe_while(
    stmt: &While,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let condition = ascribe_type(
        &stmt.condition,
        table,
        checker,
        storage,
        ctx.with_hint(TypeHint::Required(BOOL_TYPE)),
        errors,
    );
    let body = ascribe_type(
        &stmt.body,
        table,
        checker,
        storage,
        ctx.with_hint(TypeHint::Unused).in_loop(),
        errors,
    );
    TypedExpr {
        kind: ExprKind::ConditionalLoop(Loop {
            condition: Some(Box::new(coerce_condition(
                condition, table, checker, errors,
            ))),
            body: Box::new(body),
        }),
        ty: UNIT_TYPE,
        span: stmt.segment(),
    }
}

pub(super) fn ascribe_control(
    kind: ExprKind,
    span: SourceSegment,
    table: &mut VariableTable,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    if !ctx.in_loop {
        errors.push(TypeError::new(
            TypeErrorKind::ControlOutsideLoop,
            SourceLocation::new(table.path().to_owned(), span.clone()),
        ));
    }
    TypedExpr {
        kind,
        ty: NOTHING_TYPE,
        span,
    }
}
