use crate::hir::{
    ExprKind, MethodCall, Module, Redir, Redirect, Subprocess, Substitute, TypedExpr,
};
use crate::typing::lower::convert_into_string;
use crate::typing::registry::GLOB_SCHEMA;
use crate::typing::user::{
    EXITCODE_TYPE, GLOB_TYPE, INT_TYPE, PID_TYPE, STRING_TYPE, STRING_VECTOR_TYPE,
};
use crate::typing::variable::VariableTable;
use crate::typing::{ascribe_type, Context, TypeChecker, TypeError, TypeErrorKind, TypeHint};
use crate::SourceLocation;
use ast::call::{Call, Detached, Pipeline, RedirOp, Redirected};
use ast::range::FilePattern;
use ast::substitution::Substitution;
use context::source::SourceSegmentHolder;

pub(super) fn ascribe_call(
    call: &Call,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let args = call
        .arguments
        .iter()
        .map(|expr| {
            let expr = ascribe_type(expr, table, checker, storage, ctx, errors);
            if expr.ty == GLOB_TYPE {
                let glob = checker.registry[GLOB_SCHEMA]
                    .get_exact_method(
                        &checker.types,
                        &checker.registry,
                        "expand",
                        &[],
                        STRING_VECTOR_TYPE,
                    )
                    .expect("Glob schema does not have a `expand` method");
                let span = expr.span.clone();
                TypedExpr {
                    kind: ExprKind::MethodCall(MethodCall {
                        callee: Box::new(expr),
                        arguments: Vec::new(),
                        function_id: glob,
                    }),
                    ty: STRING_VECTOR_TYPE,
                    span,
                }
            } else {
                convert_into_string(expr, checker, table.path(), errors)
            }
        })
        .collect::<Vec<_>>();

    TypedExpr {
        kind: ExprKind::ProcessCall(args),
        span: call.segment(),
        ty: EXITCODE_TYPE,
    }
}

pub(super) fn ascribe_redirected(
    redirected: &Redirected,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let expr = ascribe_type(&redirected.expr, table, checker, storage, ctx, errors);
    let mut redirections = Vec::with_capacity(redirected.redirections.len());
    for redirection in &redirected.redirections {
        let operand = ascribe_type(&redirection.operand, table, checker, storage, ctx, errors);
        let operand = if matches!(redirection.operator, RedirOp::FdIn | RedirOp::FdOut) {
            if operand.ty != INT_TYPE {
                errors.push(TypeError::new(
                    TypeErrorKind::TypeMismatch {
                        expected: checker.display(INT_TYPE),
                        expected_due_to: None,
                        actual: checker.display(operand.ty),
                    },
                    SourceLocation::new(table.path().to_owned(), operand.span.clone()),
                ));
            }
            operand
        } else {
            convert_into_string(operand, checker, table.path(), errors)
        };
        redirections.push(Redir {
            fd: redirection.fd,
            operator: redirection.operator,
            operand: Box::new(operand),
        });
    }
    let ty = expr.ty;
    TypedExpr {
        kind: ExprKind::Redirect(Redirect {
            expression: Box::new(expr),
            redirections,
        }),
        ty,
        span: redirected.segment(),
    }
}

pub(super) fn ascribe_detached(
    detached: &Detached,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let expr = ascribe_type(&detached.underlying, table, checker, storage, ctx, errors);
    TypedExpr {
        kind: ExprKind::Subprocess(Subprocess {
            inner: Box::new(expr),
            awaited: false,
        }),
        ty: PID_TYPE,
        span: detached.segment(),
    }
}

pub(super) fn ascribe_pipeline(
    pipeline: &Pipeline,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let mut commands = Vec::with_capacity(pipeline.commands.len());
    for command in &pipeline.commands {
        commands.push(ascribe_type(command, table, checker, storage, ctx, errors));
    }
    TypedExpr {
        kind: ExprKind::Pipeline(commands),
        ty: EXITCODE_TYPE,
        span: pipeline.segment(),
    }
}

pub(super) fn ascribe_substitution(
    substitution: &Substitution,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let state = ctx.with_hint(TypeHint::Unused);
    let commands = substitution
        .underlying
        .expressions
        .iter()
        .map(|command| ascribe_type(command, table, checker, storage, state, errors))
        .collect::<Vec<_>>();
    TypedExpr {
        kind: match substitution.kind {
            ast::substitution::SubstitutionKind::Capture => ExprKind::Capture(commands),
            ast::substitution::SubstitutionKind::Process { direction } => {
                ExprKind::Substitute(match direction {
                    ast::substitution::Direction::Input => Substitute::In(commands),
                    ast::substitution::Direction::Output => Substitute::Out(commands),
                })
            }
        },
        ty: STRING_TYPE,
        span: substitution.segment(),
    }
}

pub(super) fn ascribe_file_pattern(
    pattern: &FilePattern,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let mut expr = ascribe_type(&pattern.pattern, table, checker, storage, ctx, errors);
    if expr.ty == STRING_TYPE {
        expr.ty = GLOB_TYPE;
    } else if expr.is_ok() {
        panic!("pattern should be of type String");
    }
    expr
}
