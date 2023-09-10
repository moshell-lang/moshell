use ast::r#type::ParametrizedType;
use context::source::SourceSegmentHolder;

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::environment::symbols::SymbolInfo;
use crate::reef::LANG_REEF;
use crate::relations::{SourceId, SymbolRef};
use crate::steps::typing::exploration::{Exploration, Links};
use crate::steps::typing::lower::call_convert_on;
use crate::types::hir::TypedExpr;
use crate::types::ty::{Type, TypeId, TypeRef};
use crate::types::{UnificationError, BOOL, ERROR, NOTHING};

/// Unifies two type identifiers, returning the type that the right hand side was unified to.
///
/// Unification is successful when the assignation type is a superset of the rvalue type, i.e
/// when the assignation type is a parent conceptually or technically of the rvalue type.
/// It is not reflexive, i.e. `unify(a, b)` is not the same as `unify(b, a)`.
///
/// A conversion may be not as simple as a reinterpretation of the value, and may require
/// a conversion function to be called. Use [`convert_expression`] to
/// generate the conversion code for a typed expression.
pub(super) fn convert_description(
    exploration: &Exploration,
    assign_to: TypeRef,
    rvalue: TypeRef,
) -> Result<TypeRef, UnificationError> {
    if assign_to.is_err() || rvalue.is_err() {
        // An error is compatible with everything, as it is a placeholder.
        return Ok(assign_to);
    }

    if rvalue == assign_to {
        // if both references are the same then no need to lookup, it's the same type
        return Ok(assign_to);
    }

    let lhs = exploration
        .get_type_ref(assign_to)
        .unwrap_or_else(|| panic!("cannot find type {assign_to:?}`"));
    let rhs = exploration
        .get_type_ref(rvalue)
        .unwrap_or_else(|| panic!("cannot find type {rvalue:?}`"));
    if lhs == rhs {
        return Ok(assign_to);
    }

    // apply the `A U Nothing => A` rule
    if *rhs == Type::Nothing {
        return Ok(assign_to);
    }

    let rvalue_typing = exploration.get_types(rvalue.reef).unwrap();

    if let Some(implicit) = rvalue_typing.implicits.get(&rvalue.type_id) {
        let implicit = exploration
            .get_type_ref(*implicit)
            .unwrap_or_else(|| panic!("cannot find type {implicit:?}`"));
        if lhs == implicit {
            return Ok(assign_to);
        }
    }
    Err(UnificationError())
}

/// Unifies multiple type identifiers in any direction.
pub(super) fn convert_many<I: IntoIterator<Item = TypeRef>>(
    exploration: &mut Exploration,
    types: I,
) -> Result<TypeRef, UnificationError> {
    let mut types = types
        .into_iter()
        .filter(|ty| ty.is_ok() && !ty.is_nothing());

    let first = types.next().unwrap_or(NOTHING);
    types.try_fold(first, |acc, ty| {
        convert_description(exploration, acc, ty)
            .or_else(|_| convert_description(exploration, ty, acc))
    })
}

/// Finds the type reference from an annotation.
pub(super) fn resolve_type_annotation(
    exploration: &mut Exploration,
    links: Links,
    type_annotation: &ast::r#type::Type,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypeRef {
    match type_annotation {
        ast::r#type::Type::Parametrized(ParametrizedType { params, .. }) => {
            let env = links.env();
            let type_symbol_ref = env.get_raw_symbol(type_annotation.segment()).unwrap();
            let main_type = match type_symbol_ref {
                SymbolRef::Local(l) => {
                    let SymbolInfo::Type(main_type) = env.symbols.get(l).unwrap().ty else {
                        panic!("type symbol is not a type")
                    };
                    main_type
                }
                SymbolRef::External(r) => {
                    let resolved_symbol = links.relations[r]
                        .state
                        .expect_resolved("unresolved type symbol during typechecking");

                    if resolved_symbol.reef == LANG_REEF {
                        let primitive_id = TypeId(resolved_symbol.object_id.0);
                        TypeRef::new(LANG_REEF, primitive_id)
                    } else {
                        let type_env = exploration.get_external_env(env, resolved_symbol).unwrap();
                        let SymbolInfo::Type(main_type) =
                            type_env.symbols.get(resolved_symbol.object_id).unwrap().ty
                        else {
                            panic!("type symbol is not a type")
                        };
                        main_type
                    }
                }
            };

            let generics = &exploration.get_description(main_type).unwrap().generics;
            if params.len() != generics.len() {
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::InvalidTypeArguments,
                        if params.len() < generics.len() {
                            format!(
                                "Missing generics for type `{}`",
                                exploration.get_type(main_type)
                            )
                        } else {
                            format!(
                                "Type `{}` were supplied {} generic argument{}",
                                exploration.get_type(main_type),
                                params.len(),
                                if params.len() == 1 { "" } else { "s" }
                            )
                        },
                    )
                    .with_observation(Observation::here(
                        links.source,
                        exploration.externals.current,
                        type_annotation.segment(),
                        format!(
                            "Expected {} generic argument{}",
                            generics.len(),
                            if generics.len() == 1 { "" } else { "s" }
                        ),
                    )),
                );
                return ERROR;
            } else if params.is_empty() {
                return main_type;
            }

            let params = params
                .iter()
                .map(|param| resolve_type_annotation(exploration, links, param, diagnostics))
                .collect();
            let instantiated_id = exploration
                .typing
                .add_type(Type::Instantiated(main_type, params));
            TypeRef::new(exploration.externals.current, instantiated_id)
        }
        ast::r#type::Type::Callable(_) => unimplemented!(),
        ast::r#type::Type::ByName(_) => unimplemented!(),
    }
}

pub(super) fn is_compatible(
    exploration: &Exploration,
    assign_to: TypeRef,
    rvalue: TypeRef,
) -> bool {
    if assign_to.is_err() || rvalue.is_err() || rvalue.is_nothing() {
        return true; // An error is compatible with everything, as it is a placeholder.
    }
    let lhs = exploration.get_type_ref(assign_to).unwrap();
    let rhs = exploration.get_type_ref(rvalue).unwrap();
    lhs == rhs
}

/// Ensures that the type annotation accepts the given value.
///
/// A type annotation might generate a conversion function call, which is returned.
pub(super) fn check_type_annotation(
    exploration: &mut Exploration,
    type_annotation: &ast::r#type::Type,
    value: TypedExpr,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedExpr {
    if value.ty.is_err() {
        return value;
    }

    let expected_type = resolve_type_annotation(exploration, links, type_annotation, diagnostics);
    let current_reef = exploration.externals.current;

    convert_expression(value, expected_type, exploration, links.source, diagnostics).unwrap_or_else(
        |mut value| {
            diagnostics.push(
                Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch")
                    .with_observation(Observation::here(
                        links.source,
                        current_reef,
                        type_annotation.segment(),
                        format!("Expected `{}`", exploration.get_type(expected_type)),
                    ))
                    .with_observation(Observation::here(
                        links.source,
                        current_reef,
                        value.segment(),
                        format!("Found `{}`", exploration.get_type(value.ty)),
                    )),
            );
            value.ty = expected_type;
            value
        },
    )
}

/// Tries to convert an expression to the given assignation type.
///
/// If unified, the expression is converted using the appropriate method.
/// If the conversion is incorrect, the input expression is returned,
/// in order to encourage the caller to report a specific error.
///
/// Most of the times, it will not generate any diagnostic, since diagnostics
/// would only be generated if an implicit conversion is incorrect (i.e. if
/// it is registered but if the appropriate method is not found).
pub(super) fn convert_expression(
    rvalue: TypedExpr,
    assign_to: TypeRef,
    exploration: &mut Exploration,
    source: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<TypedExpr, TypedExpr> {
    match convert_description(exploration, assign_to, rvalue.ty) {
        Ok(ty) => Ok(call_convert_on(
            rvalue,
            ty,
            exploration,
            |ty| format!("Cannot convert type `{ty}`"),
            diagnostics,
            source,
        )),
        Err(_) => Err(rvalue),
    }
}

/// Ensures that the expression is a boolean.
///
/// If not, a diagnostic is generated and the expression is returned.
/// Otherwise, the converted expression is returned.
pub(super) fn coerce_condition(
    condition: TypedExpr,
    exploration: &mut Exploration,
    source: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedExpr {
    match convert_expression(condition, BOOL, exploration, source, diagnostics) {
        Ok(condition) => condition,
        Err(condition) => {
            diagnostics.push(
                Diagnostic::new(DiagnosticID::TypeMismatch, "Condition must be a boolean")
                    .with_observation(Observation::here(
                        source,
                        exploration.externals.current,
                        condition.segment(),
                        format!(
                            "Type `{}` cannot be used as a condition",
                            exploration.get_type(condition.ty)
                        ),
                    )),
            );
            condition
        }
    }
}
