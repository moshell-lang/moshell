use crate::hir::{ExprKind, MethodCall, Module, TypedExpr};
use crate::typing::registry::STRING_SCHEMA;
use crate::typing::user::{UserType, STRING_TYPE};
use crate::typing::variable::VariableTable;
use crate::typing::{ascribe_type, Context, TypeChecker, TypeError, TypeErrorKind, TypeHint};
use crate::SourceLocation;
use ast::value::{LiteralValue, TemplateString};
use context::source::SourceSegmentHolder;
use std::path::Path;

pub(super) fn ascribe_template_string(
    tpl: &TemplateString,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    if tpl.parts.is_empty() {
        return TypedExpr {
            kind: ExprKind::Literal(LiteralValue::String(String::new())),
            ty: STRING_TYPE,
            span: tpl.segment(),
        };
    }

    let mut it = tpl.parts.iter().map(|part| {
        let typed_part = ascribe_type(
            part,
            table,
            checker,
            storage,
            ctx.with_hint(TypeHint::Required(STRING_TYPE)),
            errors,
        );
        convert_into_string(typed_part, checker, table.path(), errors)
    });
    let acc = it.next().unwrap();
    it.fold(acc, |acc, current| {
        let span = current.span.clone();
        TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(acc),
                arguments: vec![current],
                function_id: todo!("String concatenation"),
            }),
            ty: STRING_TYPE,
            span,
        }
    })
}

pub(super) fn convert_into_string(
    expr: TypedExpr,
    checker: &mut TypeChecker,
    path: &Path,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    match &checker.types[expr.ty] {
        UserType::Error => expr,
        UserType::Parametrized { schema, .. } => {
            if *schema == STRING_SCHEMA {
                return expr;
            }
            let schema = &checker.registry[*schema];
            if let Some(method) =
                schema.get_exact_method(&checker.registry, "to_string", &[], STRING_TYPE)
            {
                let span = expr.span.clone();
                TypedExpr {
                    kind: ExprKind::MethodCall(MethodCall {
                        callee: Box::new(expr),
                        arguments: Vec::new(),
                        function_id: method,
                    }),
                    ty: STRING_TYPE,
                    span,
                }
            } else {
                errors.push(TypeError::new(
                    TypeErrorKind::UnknownMethod {
                        name: "to_string".to_owned(),
                        type_name: checker.display(expr.ty),
                    },
                    SourceLocation::new(path.to_owned(), expr.span.clone()),
                ));
                expr
            }
        }
        _ => {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: checker.display(STRING_TYPE),
                    expected_due_to: None,
                    actual: checker.display(expr.ty),
                },
                SourceLocation::new(path.to_owned(), expr.span.clone()),
            ));
            expr
        }
    }
}
