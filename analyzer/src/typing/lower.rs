use crate::hir::{ExprKind, MethodCall, Module, TypedExpr};
use crate::typing::registry::{OPTION_SCHEMA, STRING_SCHEMA};
use crate::typing::user::{
    TypeId, UserType, BOOL_TYPE, EXITCODE_TYPE, FLOAT_TYPE, INT_TYPE, STRING_TYPE,
};
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

    let concat = checker.registry[STRING_SCHEMA]
        .get_exact_method(
            &checker.types,
            &checker.registry,
            "concat",
            &[STRING_TYPE, STRING_TYPE],
            STRING_TYPE,
        )
        .expect("String schema does not have a `concat` method");
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
                function_id: concat,
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
            if let Some(method) = schema.get_exact_method(
                &checker.types,
                &checker.registry,
                "to_string",
                &[expr.ty],
                STRING_TYPE,
            ) {
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

pub(super) fn coerce_condition(
    mut expr: TypedExpr,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    if expr.ty == EXITCODE_TYPE {
        let span = expr.span.clone();
        expr = TypedExpr {
            kind: ExprKind::Cast(Box::new(expr)),
            ty: BOOL_TYPE,
            span,
        };
    }
    match checker.types.unify(expr.ty, BOOL_TYPE) {
        Ok(_) => expr,
        Err(_) => {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: checker.display(BOOL_TYPE),
                    expected_due_to: None,
                    actual: checker.display(expr.ty),
                },
                SourceLocation::new(table.path().to_owned(), expr.span.clone()),
            ));
            expr
        }
    }
}

pub(super) struct Implicit {
    pub(super) assign_to: TypeId,
    pub(super) expected_due_to: Option<SourceLocation>,
}

impl Implicit {
    pub(super) fn new(assign_to: TypeId) -> Self {
        Self {
            assign_to,
            expected_due_to: None,
        }
    }
}

/// Lower hard-coded implicit casts.
pub(super) fn lower_implicit_cast(
    rhs: TypedExpr,
    Implicit {
        assign_to,
        expected_due_to,
    }: Implicit,
    checker: &TypeChecker,
    path: &Path,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    if checker.types.are_same(rhs.ty, assign_to) || rhs.is_err() {
        return rhs;
    }
    let span = rhs.span.clone();
    if rhs.ty == INT_TYPE && assign_to == FLOAT_TYPE {
        TypedExpr {
            kind: ExprKind::Cast(Box::new(rhs)),
            ty: assign_to,
            span,
        }
    } else if rhs.ty == EXITCODE_TYPE && assign_to == BOOL_TYPE {
        TypedExpr {
            kind: ExprKind::Cast(Box::new(rhs)),
            ty: assign_to,
            span,
        }
    } else {
        errors.push(TypeError::new(
            TypeErrorKind::TypeMismatch {
                expected: checker.display(assign_to),
                expected_due_to,
                actual: checker.display(rhs.ty),
            },
            SourceLocation::new(path.to_owned(), span),
        ));
        rhs
    }
}

/// Generates a conversion method call if needed.
pub(super) fn generate_unwrap(typed: TypedExpr, checker: &mut TypeChecker) -> TypedExpr {
    let UserType::Parametrized {
        schema: instantiated,
        ref params,
    } = checker.types[typed.ty]
    else {
        return typed;
    };
    if instantiated != OPTION_SCHEMA {
        return typed;
    }
    let return_type = *params.first().unwrap();
    let span = typed.span.clone();
    let unwrap_id = *checker.registry[OPTION_SCHEMA]
        .methods
        .get("unwrap")
        .expect("Option schema should have an `unwrap` method.");
    TypedExpr {
        kind: ExprKind::MethodCall(MethodCall {
            callee: Box::new(typed),
            arguments: vec![],
            function_id: unwrap_id,
        }),
        ty: return_type,
        span,
    }
}
