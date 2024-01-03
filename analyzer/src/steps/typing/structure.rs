use std::collections::HashMap;

use ast::r#struct::{FieldAccess, StructDeclaration};
use ast::variable::{Assign, Identifier};
use context::source::{SourceSegment, SourceSegmentHolder};

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::reef::ReefId;
use crate::relations::{LocalId, SymbolRef};
use crate::steps::typing::assign::ascribe_assign_rhs;
use crate::steps::typing::bounds::{apply_bounds, TypesBounds};
use crate::steps::typing::coercion::resolve_type_annotation;
use crate::steps::typing::exploration::{Exploration, Links};
use crate::steps::typing::{ascribe_types, ExpressionValue, TypingState};
use crate::types::engine::StructureId;
use crate::types::hir::{ExprKind, TypedExpr};
use crate::types::ty::{Field, FunctionDesc, Type, TypeId, TypeRef};
use crate::types::{hir, ERROR, UNIT};

pub(super) fn declare_structure(
    decl: &StructDeclaration,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    structure_id: StructureId,
    type_id: TypeId,
) {
    let current_reef = exploration.externals.current;

    let structure_env_id = links.source;

    let mut type_parameters = Vec::new();

    exploration
        .ctx
        .init_locals(structure_env_id, links.env().symbols.len());

    for (tparam_id, tparam) in decl.parameters.iter().enumerate() {
        let param_type_id = exploration
            .typing
            .add_type(Type::Polytype, Some(tparam.name.to_string()));
        let param_type_ref = TypeRef::new(current_reef, param_type_id);
        type_parameters.push(param_type_id);

        exploration
            .ctx
            .set_local_typed(structure_env_id, LocalId(tparam_id), param_type_ref);
        exploration
            .ctx
            .bind_name(tparam.name.to_string(), param_type_id);
    }

    // set type parameters now; they will be used by the structure's fields.
    exploration
        .type_engine
        .get_structure_mut(structure_id)
        .unwrap()
        .type_parameters = type_parameters.clone();

    let mut fields = HashMap::new();
    let mut field_types = Vec::new();

    for (field_offset, field_declaration) in decl.fields.iter().enumerate() {
        let field_type =
            resolve_type_annotation(exploration, links, &field_declaration.tpe, diagnostics);

        let local_id = LocalId(field_offset + type_parameters.len());
        exploration
            .ctx
            .set_local_typed(structure_env_id, local_id, field_type);

        field_types.push(field_type);
        fields.insert(
            field_declaration.name.to_string(),
            Field {
                ty: field_type,
                local_id,
            },
        );
    }

    // Then set the structure fields
    exploration
        .type_engine
        .get_structure_mut(structure_id)
        .unwrap()
        .fields = fields;

    // Add default constructor function
    let struct_type_ref = TypeRef::new(current_reef, type_id);
    let constructor_return_type = if type_parameters.is_empty() {
        // No need to instantiate a type if it has no type parameters,
        // directly use the base structure type
        type_id
    } else {
        exploration.typing.add_type(
            Type::Instantiated(
                struct_type_ref,
                type_parameters
                    .iter()
                    .map(|ty| TypeRef::new(current_reef, *ty))
                    .collect(),
            ),
            None,
        )
    };
    let constructor = FunctionDesc::constructor(
        type_parameters,
        field_types,
        TypeRef::new(current_reef, constructor_return_type),
    );
    let constructor_fn_id = exploration
        .type_engine
        .add_method(structure_id, "<init>", constructor);

    exploration.typing.add_type(
        Type::Function(Some(structure_env_id), constructor_fn_id),
        Some("<init>".to_string()),
    );
}

pub(super) fn ascribe_struct_declaration(
    decl: &StructDeclaration,
    exploration: &mut Exploration,
    parent_links: Links,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedExpr {
    let structure_env_id = parent_links.env().get_raw_env(decl.segment()).unwrap();
    let structure_id = exploration.type_engine.init_empty_structure();

    let type_id = exploration.typing.add_type(
        Type::Structure(Some(structure_env_id), structure_id),
        Some(decl.name.to_string()),
    );

    let links = parent_links.with_source(structure_env_id);
    declare_structure(decl, exploration, links, diagnostics, structure_id, type_id);

    let type_ref = TypeRef::new(exploration.externals.current, type_id);

    let SymbolRef::Local(structure_local_id) =
        parent_links.env().get_raw_symbol(decl.segment()).unwrap()
    else {
        unreachable!()
    };
    exploration
        .ctx
        .set_local_typed(parent_links.source, structure_local_id, type_ref);

    TypedExpr {
        kind: ExprKind::Noop,
        ty: UNIT,
        segment: decl.segment(),
    }
}

pub(super) fn ascribe_field_assign(
    assign: &Assign,
    access: &FieldAccess,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let object = ascribe_types(
        exploration,
        links,
        diagnostics,
        &access.expr,
        state.with_local_value(ExpressionValue::Unspecified),
    );
    let Some(field_match) = type_field_access(
        object.ty,
        access.field,
        access.segment(),
        links,
        exploration,
        diagnostics,
    ) else {
        return TypedExpr {
            kind: ExprKind::Noop,
            ty: ERROR,
            segment: assign.segment(),
        };
    };

    let rhs = ascribe_assign_rhs(
        assign,
        exploration,
        links,
        diagnostics,
        state.with_local_value(ExpressionValue::Expected(field_match.field_type)),
    );
    TypedExpr {
        kind: ExprKind::FieldAssign(hir::FieldAssign {
            object: Box::new(object),
            field: field_match.field,
            structure: field_match.object_structure,
            structure_reef: field_match.object_structure_reef,
            new_value: Box::new(rhs),
        }),
        ty: UNIT,
        segment: assign.segment(),
    }
}

struct FieldMatch {
    object_structure: StructureId,
    object_structure_reef: ReefId,
    field: LocalId,
    field_type: TypeRef,
}

fn type_field_access(
    object_type: TypeRef,
    field_name: Identifier,
    segment: SourceSegment,
    links: Links,
    exploration: &mut Exploration,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<FieldMatch> {
    let value_tparams = match exploration.get_type(object_type).unwrap() {
        Type::Instantiated(_, tparams) => tparams.as_slice(),
        _ => &[],
    };
    let value_base_type = exploration.get_base_type(object_type);

    let structure_id = match exploration.get_type(value_base_type).unwrap() {
        Type::Structure(_, structure_id) => *structure_id,
        _ => {
            if object_type != ERROR {
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::InvalidFieldAccess,
                        format!(
                            "could not access field `{}` on value of type `{}`",
                            field_name,
                            exploration.new_type_view(object_type, &TypesBounds::inactive())
                        ),
                    )
                    .with_observation(Observation::here(
                        links.source,
                        exploration.externals.current,
                        segment,
                        format!(
                            "`{}` is not a structured type.",
                            exploration.new_type_view(object_type, &TypesBounds::inactive())
                        ),
                    )),
                );
            }

            return None;
        }
    };

    let structure = exploration
        .get_structure(value_base_type.reef, structure_id)
        .unwrap();
    let bounds = TypesBounds::new(
        structure
            .type_parameters
            .iter()
            .map(|ty| TypeRef::new(value_base_type.reef, *ty))
            .zip(value_tparams.iter().copied())
            .collect(),
    );

    let field = structure.fields.get(field_name.value);
    match field {
        Some(field) => Some(FieldMatch {
            object_structure: structure_id,
            object_structure_reef: value_base_type.reef,
            field: field.local_id,
            field_type: apply_bounds(exploration, field.ty, &bounds),
        }),
        None => {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    format!(
                        "unknown field `{}` in structure `{}`",
                        field_name,
                        exploration.new_type_view(object_type, &TypesBounds::inactive())
                    ),
                )
                .with_observation(Observation::here(
                    links.source,
                    exploration.externals.current,
                    segment.clone(),
                    format!("`{}` does not exists", field_name),
                )),
            );
            None
        }
    }
}

pub(super) fn ascribe_field_access(
    access: &FieldAccess,
    links: Links,
    exploration: &mut Exploration,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let object = ascribe_types(exploration, links, diagnostics, &access.expr, state);
    let field_match = type_field_access(
        object.ty,
        access.field,
        access.segment(),
        links,
        exploration,
        diagnostics,
    );
    match field_match {
        None => TypedExpr {
            kind: ExprKind::Noop,
            ty: ERROR,
            segment: access.segment(),
        },
        Some(FieldMatch {
            object_structure,
            object_structure_reef,
            field,
            field_type,
        }) => TypedExpr {
            kind: ExprKind::FieldAccess(hir::FieldAccess {
                object: Box::new(object),
                structure: object_structure,
                structure_reef: object_structure_reef,
                field,
            }),
            ty: field_type,
            segment: access.segment(),
        },
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use context::source::Source;

    use crate::reef::ReefId;
    use crate::steps::typing::tests::extract_type;
    use crate::types::ty::{TypeId, TypeRef};
    use crate::types::{STRING, UNIT};

    #[test]
    fn constructor() {
        let expr = extract_type(Source::unknown(
            r#"\
            struct Test[A] {
                a: Int,
                b: A,
                c: Vec[A]
            }
            Test(7, "test", "".split(' '))
        "#,
        ));

        assert_eq!(expr, Ok(TypeRef::new(ReefId(1), TypeId(6))))
    }

    #[test]
    fn field_access() {
        let expr = extract_type(Source::unknown(
            r#"\
            struct Test[A] {
                a: Int,
                b: A,
                c: Vec[A]
            }
            Test(7, "test", "".split(' ')).b
        "#,
        ));

        assert_eq!(expr, Ok(STRING))
    }

    #[test]
    fn field_assign() {
        let expr = extract_type(Source::unknown(
            r#"\
            struct Test[A] {
                a: Int,
                b: A,
                c: Vec[A]
            }
            Test(7, "test", "".split(' ')).b = 'bar'
        "#,
        ));

        assert_eq!(expr, Ok(UNIT))
    }

    #[test]
    fn field_access_subscript() {
        let expr = extract_type(Source::unknown(
            r#"\
            struct Test[A] {
                a: Int,
                b: A,
                c: Vec[A]
            }
            Test(7, "test", "".split(' ')).c[0]
        "#,
        ));

        assert_eq!(expr, Ok(STRING))
    }

    #[test]
    fn field_assign_subscript() {
        let expr = extract_type(Source::unknown(
            r#"\
            struct Test[A] {
                a: Int,
                b: A,
                c: Vec[A]
            }
            Test(7, "test", "".split(' ')).c[0] = 'foo'
        "#,
        ));

        assert_eq!(expr, Ok(UNIT))
    }
}
