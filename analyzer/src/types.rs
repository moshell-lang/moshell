use std::collections::HashMap;

use crate::reef::LANG_REEF;
use crate::types::ty::{Type, TypeId, TypeRef};

pub(crate) mod builtin;
pub mod ctx;
pub mod engine;
pub mod hir;
pub mod operator;
pub mod ty;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefinitionId(pub(crate) TypeRef);

/// Holds all the known types.
#[derive(Default, Debug, Clone)]
pub struct Typing {
    /// The actual types.
    types: Vec<Type>,

    /// A list of implicit conversions from one type to another.
    pub(crate) implicits: HashMap<TypeId, TypeRef>,
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

    /// Gets the type with the given identifier.
    pub fn get_type(&self, type_id: TypeId) -> Option<&Type> {
        self.types.get(type_id.0)
    }
}

pub const ERROR: TypeRef = TypeRef::new(LANG_REEF, TypeId(0));
pub const NOTHING: TypeRef = TypeRef::new(LANG_REEF, TypeId(1));
pub const UNIT: TypeRef = TypeRef::new(LANG_REEF, TypeId(2));
pub const BOOL: TypeRef = TypeRef::new(LANG_REEF, TypeId(3));
pub const EXITCODE: TypeRef = TypeRef::new(LANG_REEF, TypeId(4));
pub const INT: TypeRef = TypeRef::new(LANG_REEF, TypeId(5));
pub const FLOAT: TypeRef = TypeRef::new(LANG_REEF, TypeId(6));
pub const STRING: TypeRef = TypeRef::new(LANG_REEF, TypeId(7));
pub const GENERIC_VECTOR: TypeRef = TypeRef::new(LANG_REEF, TypeId(8));
pub const POLYTYPE: TypeRef = TypeRef::new(LANG_REEF, TypeId(9));

/// An error that occurs when two types are not compatible.
#[derive(Debug, PartialEq)]
pub struct UnificationError();
