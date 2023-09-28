use ast::r#type::ParametrizedType;
use context::source::{SourceSegment, SourceSegmentHolder};

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::relations::{SourceId, SymbolRef};
use crate::steps::typing::bounds::TypesBounds;
use crate::steps::typing::exploration::{Exploration, Links};
use crate::steps::typing::lower::call_convert_on;
use crate::types::hir::TypedExpr;
use crate::types::ty::{Type, TypeRef};
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
    bounds: &mut TypesBounds,
    is_base_type: bool,
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

    if *lhs == Type::Polytype && bounds.is_self_bound(assign_to) {
        return Ok(assign_to);
    }

    let rhs = exploration
        .get_type(rvalue)
        .unwrap_or_else(|| panic!("cannot find type {rvalue:?}`"));

    // Valid excepted if both types are polytype
    if *lhs != Type::Polytype && lhs == rhs {
        return Ok(assign_to);
    }

    // apply the `A U Nothing => A` rule only if `A` is a base type
    if is_base_type && *rhs == Type::Nothing {
        return Ok(assign_to);
    }

    if let (Type::Instantiated(base_left, params_lhs), Type::Instantiated(base_right, params_rhs)) =
        (lhs, rhs)
    {
        if base_left == base_right {
            // simply test if parameters of rvalue can fit to assigned target
            // when generic parameters will have bounds, we'll probably want to
            // assign a new type that's the result of the union of rvalue and assigned.
            let are_parameters_compatible =
                params_lhs
                    .iter()
                    .zip(params_rhs)
                    .all(|(param_lhs, param_rhs)| {
                        let bound = bounds.get_bound(*param_lhs);
                        let ty = convert_description(exploration, bound, *param_rhs, bounds, false)
                            .is_ok();

                        // restrict bound even more
                        bounds.update_bounds(*param_lhs, bound, exploration);

                        ty
                    });
            if are_parameters_compatible {
                return Ok(assign_to);
            }
        }
    }

    let rvalue_typing = exploration.get_types(rvalue.reef).unwrap();

    if is_base_type {
        if let Some(implicit) = rvalue_typing.implicits.get(&rvalue.type_id) {
            let implicit = exploration
                .get_type(*implicit)
                .unwrap_or_else(|| panic!("cannot find type {implicit:?}`"));
            if lhs == implicit {
                return Ok(assign_to);
            }
        }
    }
    Err(UnificationError())
}

/// Unifies multiple type identifiers in any direction.
pub(super) fn convert_many<I: IntoIterator<Item = TypeRef>>(
    exploration: &mut Exploration,
    bounds: &mut TypesBounds,
    types: I,
) -> Result<TypeRef, UnificationError> {
    let mut types = types
        .into_iter()
        .filter(|ty| ty.is_ok() && !ty.is_nothing());

    let first = types.next().unwrap_or(NOTHING);
    types.try_fold(first, |acc, ty| {
        convert_description(exploration, acc, ty, bounds, true)
            .or_else(|_| convert_description(exploration, ty, acc, bounds, true))
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
            let type_variable = match type_symbol_ref {
                SymbolRef::Local(local) => exploration.ctx.get_local(links.source, local).unwrap(),
                SymbolRef::External(r) => {
                    let resolved_symbol = links.relations[r]
                        .state
                        .expect_resolved("unresolved type symbol during typechecking");

                    let ctx = if resolved_symbol.reef == exploration.externals.current {
                        &exploration.ctx
                    } else {
                        &exploration
                            .externals
                            .get_reef(resolved_symbol.reef)
                            .unwrap()
                            .type_context
                    };
                    ctx.get_local(resolved_symbol.source, resolved_symbol.object_id)
                        .unwrap()
                }
            };
            let main_type = type_variable.type_ref;

            let generics = &exploration
                .get_description(main_type)
                .map(|d| d.generics.as_slice())
                .unwrap_or(&[]);
            if params.len() != generics.len() {
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::InvalidTypeArguments,
                        if params.len() < generics.len() {
                            format!(
                                "Missing generics for type `{}`",
                                exploration.new_type_view(main_type,  &TypesBounds::inactive()),
                            )
                        } else {
                            format!(
                                "Type `{}` were supplied {} generic argument{}",
                                exploration.new_type_view(main_type,  &TypesBounds::inactive()),
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
                .add_type(Type::Instantiated(main_type, params), None);
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
    let lhs = exploration.get_type(assign_to).unwrap();
    let rhs = exploration.get_type(rvalue).unwrap();
    lhs == rhs
}

/// Ensures that the type annotation accepts the given value.
///
/// A type annotation might generate a conversion function call, which is returned.
pub(super) fn check_type_annotation(
    exploration: &mut Exploration,
    expected_type: TypeRef,
    expected_type_segment: SourceSegment,
    bounds: &mut TypesBounds,
    value: TypedExpr,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedExpr {
    if value.ty.is_err() {
        return value;
    }

    let current_reef = exploration.externals.current;

    convert_expression(
        value,
        expected_type,
        bounds,
        exploration,
        links.source,
        diagnostics,
    )
    .unwrap_or_else(|mut value| {
        diagnostics.push(
            Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch")
                .with_observation(Observation::here(
                    links.source,
                    current_reef,
                    expected_type_segment,
                    format!(
                        "Expected `{}`",
                        exploration.new_type_view(expected_type, bounds),
                    ),
                ))
                .with_observation(Observation::here(
                    links.source,
                    current_reef,
                    value.segment(),
                    format!("Found `{}`", exploration.new_type_view(value.ty, bounds)),
                )),
        );
        value.ty = expected_type;
        value
    })
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
    bounds: &mut TypesBounds,
    exploration: &mut Exploration,
    source: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<TypedExpr, TypedExpr> {
    match convert_description(exploration, assign_to, rvalue.ty, bounds, true) {
        Ok(ty) => Ok(call_convert_on(
            rvalue,
            ty,
            exploration,
            |ty| {
                format!(
                    "Cannot convert type `{}`",
                    exploration.new_type_view(ty, &TypesBounds::inactive())
                )
            },
            diagnostics,
            bounds,
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
    match convert_expression(
        condition,
        BOOL,
        &mut TypesBounds::inactive(),
        exploration,
        source,
        diagnostics,
    ) {
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
                            exploration.new_type_view(condition.ty, &TypesBounds::inactive()),
                        ),
                    )),
            );
            condition
        }
    }
}
