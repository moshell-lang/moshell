use crate::hir::{ExprKind, Module, TypedExpr};
use crate::typing::function::Function;
use crate::typing::registry::{FunctionId, Registry};
use crate::typing::user::{TypeArena, UserType};
use crate::typing::variable::{LocalId, VariableTable};
use crate::typing::{
    ascribe_type, Context, Parameter, TypeChecker, TypeError, TypeErrorKind, TypeId,
};
use crate::{hir, SourceLocation};
use ast::r#struct::FieldAccess;
use context::source::SourceSegmentHolder;
use std::collections::HashMap;

/// A structure definition, describing a type with fields and methods.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Schema {
    /// The display name of the schema.
    pub name: String,

    /// The [`crate::typing::user::UserType::GenericVariable`]s used.
    pub generic_variables: Vec<TypeId>,

    /// The fields and their types.
    pub fields: Vec<SchemaField>,

    /// The methods and their types.
    pub methods: HashMap<String, FunctionId>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SchemaField {
    pub name: String,
    pub param: Parameter,
}

impl Schema {
    /// Creates a new schema.
    pub fn new(name: String) -> Self {
        Self {
            name,
            generic_variables: Vec::new(),
            fields: Vec::new(),
            methods: HashMap::new(),
        }
    }

    /// Creates a new generic schema.
    pub fn generic(name: String, generic_variables: Vec<TypeId>) -> Self {
        Self {
            name,
            generic_variables,
            fields: Vec::new(),
            methods: HashMap::new(),
        }
    }

    /// Finds a method that have those exact name, parameters and return type.
    pub fn get_exact_method(
        &self,
        types: &TypeArena,
        registry: &Registry,
        name: &str,
        params: &[TypeId],
        return_ty: TypeId,
    ) -> Option<FunctionId> {
        self.methods.get(name).and_then(|&id| {
            let func = &registry[id];
            if func
                .param_types
                .iter()
                .map(|param| param.ty)
                .zip(params.iter())
                .all(|(param_ty, ty)| types.are_same(param_ty, *ty))
                && types.are_same(func.return_type, return_ty)
            {
                Some(id)
            } else {
                None
            }
        })
    }
}

pub(super) fn ascribe_field_access(
    FieldAccess {
        expr,
        field: field_name,
        segment: span,
    }: &FieldAccess,
    table: &mut VariableTable,
    checker: &mut TypeChecker,
    storage: &mut Module,
    ctx: Context,
    errors: &mut Vec<TypeError>,
) -> TypedExpr {
    let typed_expr = ascribe_type(expr, table, checker, storage, ctx, errors);
    if typed_expr.is_err() {
        return typed_expr;
    }
    let UserType::Parametrized { schema, ref params } = checker.types[typed_expr.ty] else {
        errors.push(TypeError::new(
            TypeErrorKind::TypeMismatch {
                expected: "Struct".to_string(),
                expected_due_to: None,
                actual: checker.display(typed_expr.ty),
            },
            SourceLocation::new(table.path().to_owned(), span.clone()),
        ));
        return TypedExpr::error(span.clone());
    };
    let Schema {
        generic_variables,
        fields,
        methods,
        ..
    } = &checker.registry[schema];
    let Some(field_id) = fields
        .iter()
        .position(|field| field.name == field_name.value)
    else {
        errors.push(TypeError::new(
            if let Some(method) = methods.get(field_name.value.as_str()) {
                let Function {
                    ref param_types, ..
                } = checker.registry[*method];
                let mut builder = "(".to_owned();
                for param in param_types {
                    if param.ty == typed_expr.ty {
                        continue;
                    }
                    if builder.ends_with('(') {
                        builder.push('_');
                    } else {
                        builder.push_str(", _");
                    }
                }
                builder.push(')');
                TypeErrorKind::MethodLikeFieldAccess {
                    name: field_name.value.to_string(),
                    parentheses: builder,
                }
            } else {
                TypeErrorKind::UnknownField {
                    name: field_name.value.to_string(),
                    type_name: checker.display(typed_expr.ty),
                    available: fields.iter().map(|field| field.name.clone()).collect(),
                }
            },
            SourceLocation::new(table.path().to_owned(), field_name.segment()),
        ));
        return TypedExpr::error(span.clone());
    };
    let field = &fields[field_id];
    let params = params.clone();
    let field_ty = checker
        .types
        .concretize(field.param.ty, generic_variables, &params);
    TypedExpr {
        kind: ExprKind::FieldAccess(hir::FieldAccess {
            object: Box::new(typed_expr),
            structure: schema,
            field: LocalId(field_id),
        }),
        span: span.clone(),
        ty: field_ty,
    }
}

#[cfg(test)]
mod tests {
    use crate::typing::tests::type_check;
    use crate::typing::{TypeError, TypeErrorKind};
    use crate::SourceLocation;
    use std::path::PathBuf;

    #[test]
    fn instantiate_structure_incorrect_field() {
        let errors = type_check(
            "struct Range { start: Int, end: Int }
            Range(1, 'a')
            ",
        );
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "Int".to_owned(),
                    expected_due_to: Some(SourceLocation::new(PathBuf::from("main.msh"), 32..35)),
                    actual: "String".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 59..62),
            )]
        );
    }

    #[test]
    fn instantiate_generic_structure() {
        let errors = type_check(
            "struct Pair[A, B] { first: A, second: B }
            Pair::[Int, String](1, 'a')
            ",
        );
        assert_eq!(errors, []);
    }

    #[test]
    fn instantiate_structure_with_concretized_type() {
        let errors = type_check(
            "struct Box[T] {}
            struct Foo { box: Box[Int] }
            Foo(Box())
            ",
        );
        assert_eq!(errors, []);
    }

    #[test]
    fn field_assign_incorrect_type() {
        let errors = type_check(
            "struct Box { item: Bool }
            val box = Box(true)
            $box.item = 5
            ",
        );
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::TypeMismatch {
                    expected: "Bool".to_owned(),
                    expected_due_to: None,
                    actual: "Int".to_owned(),
                },
                SourceLocation::new(PathBuf::from("main.msh"), 74..83),
            )]
        );
    }
}
