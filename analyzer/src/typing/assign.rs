use crate::hir::{ExprKind, FieldAssign, LocalAssignment, MethodCall, Module, TypedExpr};
use crate::symbol::{SymbolRegistry, UndefinedSymbol};
use crate::typing::function::Function;
use crate::typing::schema::ascribe_field_access;
use crate::typing::user::{UserType, UNIT_TYPE};
use crate::typing::variable::VariableTable;
use crate::typing::{ascribe_type, Context, TypeChecker, TypeError, TypeErrorKind, TypeHint};
use crate::{hir, SourceLocation};
use ast::operation::{BinaryOperation, BinaryOperator};
use ast::r#struct::FieldAccess;
use ast::r#use::InclusionPathItem;
use ast::range::Subscript;
use ast::variable::{Assign, AssignOperator, Path, VarName, VarReference};
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
                ty: UNIT_TYPE,
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

pub(super) fn ascribe_subscript(
    subscript: &Subscript,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let target = ascribe_type(
        &subscript.target,
        table,
        checker,
        storage,
        ctx.with_hint(TypeHint::Used),
        errors,
    );
    let index = ascribe_type(
        &subscript.index,
        table,
        checker,
        storage,
        ctx.with_hint(TypeHint::Used),
        errors,
    );
    if target.is_err() || index.is_err() {
        return TypedExpr::error(subscript.segment());
    }
    let UserType::Parametrized { schema, params } = checker.types[target.ty].clone() else {
        panic!("Expected a parametrized type");
    };
    let mut generics = checker.registry[schema].generic_variables.clone();
    let name = "[]";
    let Some(method_id) = checker.registry[schema].methods.get(name).copied() else {
        errors.push(TypeError::new(
            TypeErrorKind::UnknownMethod {
                name: name.to_owned(),
                type_name: checker.display(target.ty),
            },
            SourceLocation::new(table.path().to_owned(), subscript.segment()),
        ));
        return TypedExpr::error(subscript.segment());
    };
    let Function {
        ref generic_variables,
        ref param_types,
        return_type,
        ..
    } = checker.registry[method_id];
    generics.extend(generic_variables);
    let return_type = checker.types.concretize(return_type, &generics, &params);
    let [_self_param, index_param] = param_types.as_slice() else {
        errors.push(TypeError::new(
            TypeErrorKind::ArityMismatch {
                expected: param_types.len(),
                received: 1,
            },
            SourceLocation::new(table.path().to_owned(), subscript.segment()),
        ));
        return TypedExpr::error(subscript.segment());
    };
    match checker.types.unify(index_param.ty, index.ty) {
        Ok(_) => TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(target),
                arguments: vec![index],
                function_id: method_id,
            }),
            ty: return_type,
            span: subscript.segment(),
        },
        Err(_) => {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: checker.display(index_param.ty),
                    expected_due_to: None,
                    actual: checker.display(index.ty),
                },
                SourceLocation::new(table.path().to_owned(), index.span),
            ));
            TypedExpr::error(subscript.segment())
        }
    }
}

/// Creates the right hand side of an assignment.
///
/// The state should contain the [`TypeHint::Required`] value of the left hand side. If not, the
/// type of the right hand side will not be checked.
fn ascribe_assign_rhs(
    assign: &Assign,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let expr = match assign.operator {
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
    };
    if let TypeHint::Required(ty) = ctx.hint {
        if let Err(_) = checker.types.unify(expr.ty, ty) {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: checker.display(ty),
                    expected_due_to: None,
                    actual: checker.display(expr.ty),
                },
                SourceLocation::new(table.path().to_owned(), assign.segment()),
            ));
        }
    }
    expr
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
    let TypedExpr {
        kind:
            ExprKind::FieldAccess(hir::FieldAccess {
                object,
                structure,
                field,
            }),
        ty: field_ty,
        span: _,
    } = ascribe_field_access(field, table, checker, module, ctx, errors)
    else {
        return TypedExpr::error(assign.segment());
    };
    let ctx = ctx.with_hint(TypeHint::Required(field_ty));
    let new_value = ascribe_assign_rhs(assign, table, checker, module, ctx, errors);
    TypedExpr {
        kind: ExprKind::FieldAssign(FieldAssign {
            object,
            structure,
            field,
            new_value: Box::new(new_value),
        }),
        ty: UNIT_TYPE,
        span: assign.segment(),
    }
}

pub(super) fn ascribe_identifier(
    ident: &Path,
    table: &mut VariableTable,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    assert_eq!(ident.path.len(), 1);
    ascribe_var_reference(
        &VarReference {
            name: VarName::User(ident.path.last().unwrap().name().into()),
            segment: ident.segment(),
        },
        table,
        errors,
    )
}

pub(super) fn ascribe_var_reference(
    VarReference {
        name,
        segment: span,
    }: &VarReference,
    table: &mut VariableTable,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    match table.lookup_variable(name.name()) {
        Ok(var) => TypedExpr {
            kind: ExprKind::Reference(var.id.clone()),
            span: span.clone(),
            ty: var.ty,
        },
        Err(err) => {
            errors.push(TypeError::new(
                TypeErrorKind::UndefinedSymbol {
                    name: name.name().to_owned(),
                    expected: SymbolRegistry::Variable,
                    found: err.into(),
                },
                SourceLocation::new(table.path().to_owned(), span.clone()),
            ));
            TypedExpr::error(span.clone())
        }
    }
}
