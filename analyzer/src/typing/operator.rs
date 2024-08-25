use crate::hir::{ExprKind, MethodCall, Module, TypedExpr};
use crate::typing::function::Function;
use crate::typing::user::UserType;
use crate::typing::variable::VariableTable;
use crate::typing::{ascribe_type, Context, TypeChecker, TypeError, TypeErrorKind};
use crate::SourceLocation;
use ast::operation::{BinaryOperation, BinaryOperator};
use context::source::SourceSegmentHolder;

pub(super) fn ascribe_binary(
    binary: &BinaryOperation,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let left = ascribe_type(&binary.left, table, checker, storage, ctx, errors);
    let right = ascribe_type(&binary.right, table, checker, storage, ctx, errors);
    if left.is_err() || right.is_err() {
        return TypedExpr::error(binary.segment());
    }
    let UserType::Parametrized { schema, params: _ } = checker.types[left.ty] else {
        panic!("Expected a parametrized type");
    };
    let name = name_binary_method(binary.op);
    let Some(method_id) = checker.registry[schema].methods.get(name).copied() else {
        errors.push(TypeError::new(
            TypeErrorKind::UnknownMethod {
                name: name.to_owned(),
                type_name: checker.display(left.ty),
            },
            SourceLocation::new(table.path().to_owned(), binary.segment()),
        ));
        return TypedExpr::error(binary.segment());
    };
    let Function {
        ref param_types,
        return_type,
        ..
    } = checker.registry[method_id];
    let [self_param, param] = param_types.as_slice() else {
        errors.push(TypeError::new(
            TypeErrorKind::ArityMismatch {
                expected: param_types.len(),
                received: 1,
            },
            SourceLocation::new(table.path().to_owned(), binary.segment()),
        ));
        return TypedExpr::error(binary.segment());
    };
    if let Err(_) = checker.types.unify(self_param.ty, left.ty) {
        errors.push(TypeError::new(
            TypeErrorKind::TypeMismatch {
                expected: checker.display(self_param.ty),
                expected_due_to: None,
                actual: checker.display(left.ty),
            },
            SourceLocation::new(table.path().to_owned(), left.span),
        ));
        return TypedExpr::error(binary.segment());
    }
    match checker.types.unify(left.ty, param.ty) {
        Ok(_) => TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(left),
                arguments: vec![right],
                function_id: method_id,
            }),
            ty: return_type,
            span: binary.segment(),
        },
        Err(_) => {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: checker.display(param.ty),
                    expected_due_to: None,
                    actual: checker.display(right.ty),
                },
                SourceLocation::new(table.path().to_owned(), right.span),
            ));
            TypedExpr::error(binary.segment())
        }
    }
}

fn name_binary_method(op: BinaryOperator) -> &'static str {
    use BinaryOperator as Op;
    match op {
        Op::Plus => "add",
        Op::Minus => "sub",
        Op::Times => "mul",
        Op::Divide => "div",
        Op::Modulo => "mod",
        Op::And => "and",
        Op::Or => "or",
        Op::EqualEqual => "eq",
        Op::NotEqual => "ne",
        Op::Less => "lt",
        Op::LessEqual => "le",
        Op::Greater => "gt",
        Op::GreaterEqual => "ge",
    }
}
