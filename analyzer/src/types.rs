use crate::types::hir::TypeId;
use crate::types::ty::Type;
use std::collections::HashMap;

pub mod ctx;
pub mod engine;
pub mod hir;
pub mod ty;

/// Holds all the known types.
#[derive(Default)]
pub struct Typing {
    /// The actual types.
    types: Vec<Type>,

    /// A list of implicit conversions from one type to another.
    implicits: HashMap<TypeId, TypeId>,
}

impl Typing {
    /// Constructs a new typing context with the built-in types.
    pub fn lang() -> Self {
        Self {
            types: vec![
                Type::Error,
                Type::Nothing,
                Type::Bool,
                Type::Int,
                Type::Float,
                Type::String,
            ],
            implicits: HashMap::from([(INT, FLOAT)]),
        }
    }

    /// Unifies two types, returning the type that the right hand side was unified to.
    pub(crate) fn unify(
        &mut self,
        assign_to: TypeId,
        rvalue: TypeId,
    ) -> Result<TypeId, UnificationError> {
        let lhs = &self.types[assign_to.0];
        let rhs = &self.types[rvalue.0];
        if lhs == rhs {
            return Ok(assign_to);
        }
        if let Some(implicit) = self.implicits.get(&rvalue) {
            if lhs == &self.types[implicit.0] {
                return Ok(assign_to);
            }
        }
        Err(UnificationError())
    }

    pub fn unify_many(
        &mut self,
        types: impl Iterator<Item = TypeId>,
    ) -> Result<TypeId, UnificationError> {
        let mut types = types.peekable();
        let first = types.next().unwrap();
        types.try_fold(first, |acc, ty| {
            self.unify(acc, ty).or_else(|_| self.unify(ty, acc))
        })
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

pub const ERROR: TypeId = TypeId(0);
pub const NOTHING: TypeId = TypeId(1);
pub const BOOL: TypeId = TypeId(2);
pub const INT: TypeId = TypeId(3);
pub const FLOAT: TypeId = TypeId(4);
pub const STRING: TypeId = TypeId(5);

/// An error that occurs when two types are not compatible.
#[derive(Debug, PartialEq)]
pub struct UnificationError();
