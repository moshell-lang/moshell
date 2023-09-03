use crate::environment::symbols::SymbolInfo;
use crate::reef::{ReefId, LANG_REEF};
use crate::relations::{SourceId, SymbolRef};
use crate::steps::typing::exploration::UniversalReefAccessor;
use crate::types::ty::{Type, TypeId, TypeRef};
use ast::r#use::InclusionPathItem;
use context::source::SourceSegmentHolder;
use std::collections::HashMap;

pub(crate) mod builtin;
pub mod ctx;
pub mod engine;
pub mod hir;
pub mod operator;
pub mod ty;

/// Holds all the known types.
#[derive(Default)]
pub struct Typing {
    /// The actual types.
    types: Vec<Type>,

    /// A list of implicit conversions from one type to another.
    implicits: HashMap<TypeId, TypeRef>,
}

impl Typing {
    pub(crate) fn set_implicit_conversion(&mut self, from: TypeId, to: TypeRef) {
        self.implicits.insert(from, to);
    }
    pub(crate) fn add_type(&mut self, ty: Type) -> TypeId {
        let type_id = TypeId(self.types.len());
        self.types.push(ty);
        type_id
    }

    pub(crate) fn get_type(&self, type_id: TypeId) -> Option<&Type> {
        self.types.get(type_id.0)
    }
}

pub const ERROR: TypeRef = TypeRef::new(LANG_REEF, TypeId(0));
pub const NOTHING: TypeRef = TypeRef::new(LANG_REEF, TypeId(1));
pub const UNIT: TypeRef = TypeRef::new(LANG_REEF, TypeId(2));
pub const BOOL: TypeRef = TypeRef::new(LANG_REEF, TypeId(3));
pub const EXIT_CODE: TypeRef = TypeRef::new(LANG_REEF, TypeId(4));
pub const INT: TypeRef = TypeRef::new(LANG_REEF, TypeId(5));
pub const FLOAT: TypeRef = TypeRef::new(LANG_REEF, TypeId(6));
pub const STRING: TypeRef = TypeRef::new(LANG_REEF, TypeId(7));

/// Unifies two type identifiers, returning the type that the right hand side was unified to.
///
/// Unification is successful when the assignation type is a superset of the rvalue type, i.e
/// when the assignation type is a parent conceptually or technically of the rvalue type.
/// It is not reflexive, i.e. `unify(a, b)` is not the same as `unify(b, a)`.
///
/// A conversion may be not as simple as a reinterpretation of the value, and may require
/// a conversion function to be called. Use [`crate::steps::typing::convert_expression`] to
/// generate the conversion code for a typed expression.
pub fn convert_description(
    ura: &UniversalReefAccessor,
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

    let lhs = get_type(assign_to, ura).unwrap_or_else(|| panic!("cannot find type {assign_to:?}`"));
    let rhs = get_type(rvalue, ura).unwrap_or_else(|| panic!("cannot find type {rvalue:?}`"));
    if lhs == rhs {
        return Ok(assign_to);
    }

    // apply the `A U Nothing => A` rule
    if *rhs == Type::Nothing {
        return Ok(assign_to);
    }

    let rvalue_typing = ura.get_types(rvalue.reef).unwrap().typing;

    if let Some(implicit) = rvalue_typing.implicits.get(&rvalue.type_id) {
        let implicit =
            get_type(*implicit, ura).unwrap_or_else(|| panic!("cannot find type {implicit:?}`"));
        if lhs == implicit {
            return Ok(assign_to);
        }
    }
    Err(UnificationError())
}

/// Unifies multiple type identifiers in any direction.
pub fn convert_many<I: IntoIterator<Item = TypeRef>>(
    ura: &UniversalReefAccessor,
    types: I,
) -> Result<TypeRef, UnificationError> {
    let mut types = types
        .into_iter()
        .filter(|ty| ty.is_ok() && !ty.is_nothing());

    let first = types.next().unwrap_or(NOTHING);
    types.try_fold(first, |acc, ty| {
        convert_description(ura, acc, ty).or_else(|_| convert_description(ura, ty, acc))
    })
}

pub fn get_type<'a>(type_ref: TypeRef, ura: &'a UniversalReefAccessor) -> Option<&'a Type> {
    ura.get_types(type_ref.reef)
        .and_then(|types| types.typing.get_type(type_ref.type_id))
}

/// Finds the type reference from an annotation.
pub(crate) fn resolve_type(
    ura: &UniversalReefAccessor,
    reef_id: ReefId,
    env_id: SourceId,
    type_annotation: &ast::r#type::Type,
) -> TypeRef {
    match type_annotation {
        ast::r#type::Type::Parametrized(param) => {
            if param.path.len() > 1 || !param.params.is_empty() {
                unimplemented!();
            }
            if let InclusionPathItem::Symbol(_, _) = param
                .path
                .first()
                .expect("Type annotation should not be empty")
            {
                let engine = ura.get_engine(reef_id).unwrap();
                let relations = ura.get_relations(reef_id).unwrap();

                let env = engine.get_environment(env_id).unwrap();
                let type_symbol_ref = env.get_raw_symbol(type_annotation.segment()).unwrap();
                let type_symbol = match type_symbol_ref {
                    SymbolRef::Local(l) => env.symbols.get(l).unwrap(),
                    SymbolRef::External(r) => {
                        let resolved_symbol = relations[r]
                            .state
                            .expect_resolved("unresolved type symbol during typechecking");

                        if resolved_symbol.reef == LANG_REEF {
                            let primitive_id = TypeId(resolved_symbol.object_id.0);
                            return TypeRef::new(LANG_REEF, primitive_id);
                        }

                        let type_env = ura
                            .get_engine(resolved_symbol.reef)
                            .unwrap()
                            .get_environment(resolved_symbol.source)
                            .unwrap();
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
            } else {
                unimplemented!("type's path cannot start with `reef` yet")
            }
        }
        ast::r#type::Type::Callable(_) => unimplemented!(),
        ast::r#type::Type::ByName(_) => unimplemented!(),
    }
}

/// An error that occurs when two types are not compatible.
#[derive(Debug, PartialEq)]
pub struct UnificationError();
