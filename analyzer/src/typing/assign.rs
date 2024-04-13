use crate::hir::{ExprKind, LocalAssignment, Module, TypedExpr};
use crate::symbol::{SymbolRegistry, UndefinedSymbol};
use crate::typing::variable::VariableTable;
use crate::typing::{ascribe_type, Context, TypeChecker, TypeError, TypeErrorKind, TypeHint};
use crate::SourceLocation;
use ast::operation::{BinaryOperation, BinaryOperator};
use ast::r#struct::FieldAccess;
use ast::r#use::InclusionPathItem;
use ast::range::Subscript;
use ast::variable::{Assign, AssignOperator};
use ast::Expr;
use context::source::SourceSegmentHolder;

pub(super) fn ascribe_assign(
    assign: &Assign,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    if let Expr::Subscript(sub) = assign.left.as_ref() {
        return ascribe_assign_subscript(assign, sub, table, checker, storage, ctx, errors);
    }
    if let Expr::FieldAccess(field) = assign.left.as_ref() {
        return ascribe_field_assign(assign, field, table, checker, storage, ctx, errors);
    }

    let left = match assign.left.as_ref() {
        Expr::VarReference(var) => table.lookup_variable(var.name.name()),
        Expr::Path(path) => {
            if let [InclusionPathItem::Symbol(ident)] = path.path.as_slice() {
                table.lookup_variable(ident.value.as_str())
            } else {
                Err(UndefinedSymbol::NotFound)
            }
        }
        _ => Err(UndefinedSymbol::NotFound),
    };

    let rhs = ascribe_assign_rhs(
        assign,
        table,
        checker,
        storage,
        ctx.with_hint(
            left.as_ref()
                .map_or(TypeHint::Used, |var| TypeHint::Required(var.ty)),
        ),
        errors,
    );
    match left {
        Ok(var) => {
            if let Err(_) = checker.types.unify(rhs.ty, var.ty) {
                errors.push(TypeError::new(
                    TypeErrorKind::TypeMismatch {
                        expected: checker.display(var.ty),
                        expected_due_to: None,
                        actual: checker.display(rhs.ty),
                    },
                    SourceLocation::new(table.path().to_owned(), assign.segment()),
                ));
            }
            if !var.can_reassign {
                errors.push(TypeError::new(
                    TypeErrorKind::CannotReassign {
                        name: assign.name().unwrap_or_default(),
                    },
                    SourceLocation::new(table.path().to_owned(), assign.segment()),
                ));
            }
            TypedExpr {
                kind: ExprKind::LocalAssign(LocalAssignment {
                    identifier: var.id,
                    rhs: Box::new(rhs),
                }),
                ty: var.ty,
                span: assign.segment(),
            }
        }
        Err(err) => {
            errors.push(TypeError::new(
                TypeErrorKind::UndefinedSymbol {
                    name: assign.name().unwrap_or_default(),
                    expected: SymbolRegistry::Variable,
                    found: err.into(),
                },
                SourceLocation::new(table.path().to_owned(), assign.segment()),
            ));
            TypedExpr::error(assign.left.segment())
        }
    }
}

/// Creates the right hand side of an assignment.
///
/// The state should contain the [`ExpressionValue::Expected`] value of the left hand side.
fn ascribe_assign_rhs(
    assign: &Assign,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    match assign.operator {
        AssignOperator::Assign => ascribe_type(&assign.value, table, checker, storage, ctx, errors),
        operator => {
            let binary = Expr::Binary(BinaryOperation {
                left: assign.left.clone(),
                op: BinaryOperator::try_from(operator).expect("Invalid assign operator"),
                right: assign.value.clone(),
            });
            ascribe_type(
                &binary,
                table,
                checker,
                storage,
                ctx.with_hint(TypeHint::Used),
                errors,
            )
        }
    }
}

fn ascribe_assign_subscript(
    assign: &Assign,
    sub: &Subscript,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    module: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    todo!()
}

fn ascribe_field_assign(
    assign: &Assign,
    field: &FieldAccess,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    module: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    todo!()
}
