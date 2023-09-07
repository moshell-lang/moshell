use context::source::SourceSegmentHolder;

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::environment::symbols::SymbolInfo;
use crate::reef::LANG_REEF;
use crate::relations::{SourceId, SymbolRef};
use crate::steps::typing::exploration::{Exploration, Links};
use crate::steps::typing::lower::call_convert_on;
use crate::types::hir::TypedExpr;
use crate::types::ty::{Type, TypeId, TypeRef};
use crate::types::{UnificationError, BOOL, NOTHING};

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
    exploration: &Exploration, // TODO: &mut ?
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
        .get_type(assign_to)
        .unwrap_or_else(|| panic!("cannot find type {assign_to:?}`"));
    let rhs = exploration
        .get_type(rvalue)
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
            .get_type(*implicit)
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
pub(super) fn resolve_type(
    exploration: &mut Exploration,
    links: Links,
    type_annotation: &ast::r#type::Type,
    _diagnostics: &mut [Diagnostic],
) -> TypeRef {
    match type_annotation {
        ast::r#type::Type::Parametrized(param) => {
            if !param.params.is_empty() {
                unimplemented!();
            }
            let env = links.env();
            let type_symbol_ref = env.get_raw_symbol(type_annotation.segment()).unwrap();
            let type_symbol = match type_symbol_ref {
                SymbolRef::Local(l) => env.symbols.get(l).unwrap(),
                SymbolRef::External(r) => {
                    let resolved_symbol = links.relations[r]
                        .state
                        .expect_resolved("unresolved type symbol during typechecking");

                    if resolved_symbol.reef == LANG_REEF {
                        let primitive_id = TypeId(resolved_symbol.object_id.0);
                        return TypeRef::new(LANG_REEF, primitive_id);
                    }

                    let type_env = exploration.get_external_env(env, resolved_symbol).unwrap();
                    type_env.symbols.get(resolved_symbol.object_id).unwrap()
                }
            };

            if let SymbolInfo::Type(type_ref) = type_symbol.ty {
                type_ref
            } else {
                panic!(
                    "type {type_annotation} refers to a {} symbol ",
                    type_symbol.ty
                )
            }
        }
        ast::r#type::Type::Callable(_) => unimplemented!(),
        ast::r#type::Type::ByName(_) => unimplemented!(),
    }
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

    let expected_type = resolve_type(exploration, links, type_annotation, diagnostics);
    let current_reef = exploration.externals.current;

    convert_expression(value, expected_type, exploration, links.source, diagnostics).unwrap_or_else(
        |value| {
            diagnostics.push(
                Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch")
                    .with_observation(Observation::here(
                        links.source,
                        current_reef,
                        type_annotation.segment(),
                        format!(
                            "Expected `{}`",
                            exploration.get_type(expected_type).unwrap()
                        ),
                    ))
                    .with_observation(Observation::here(
                        links.source,
                        current_reef,
                        value.segment(),
                        format!("Found `{}`", exploration.get_type(value.ty).unwrap()),
                    )),
            );
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
                            exploration.get_type(condition.ty).unwrap()
                        ),
                    )),
            );
            condition
        }
    }
}
