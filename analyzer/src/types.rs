use std::collections::HashMap;

use crate::reef::LANG_REEF;
use crate::types::ty::{Type, TypeId, TypeRef};

pub(crate) mod builtin;
pub mod ctx;
pub mod engine;
pub mod hir;
pub mod operator;
pub mod ty;

/// Holds all the known types.
#[derive(Default, Debug, Clone)]
pub struct Typing {
    /// The actual types, bound with an optional name.
    types: Vec<(Type, Option<String>)>,

    /// A list of implicit conversions from one type to another.
    pub(crate) implicits: HashMap<TypeId, TypeRef>,
}

impl Typing {
    pub(crate) fn set_implicit_conversion(&mut self, from: TypeId, to: TypeRef) {
        self.implicits.insert(from, to);
    }

    pub(crate) fn add_type(&mut self, ty: Type, name: Option<String>) -> TypeId {
        let type_id = TypeId(self.types.len());
        self.types.push((ty, name));
        type_id
    }

    /// Gets the type with the given identifier.
    pub fn get_type(&self, type_id: TypeId) -> Option<&Type> {
        self.types.get(type_id.0).map(|(t, _)| t)
    }

    pub fn get_type_name(&self, type_id: TypeId) -> Option<&String> {
        self.types
            .get(type_id.0)
            .and_then(|(_, name)| name.as_ref())
    }

    pub fn iter(&self) -> impl Iterator<Item = (TypeId, &Type)> {
        self.types
            .iter()
            .enumerate()
            .map(|(idx, (tpe, _))| (TypeId(idx), tpe))
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
pub const GENERIC_OPTION: TypeRef = TypeRef::new(LANG_REEF, TypeId(9));
pub const GLOB: TypeRef = TypeRef::new(LANG_REEF, TypeId(10));
pub const PID: TypeRef = TypeRef::new(LANG_REEF, TypeId(11));

/// An error that occurs when two types are not compatible.
#[derive(Debug, PartialEq)]
pub struct UnificationError();
