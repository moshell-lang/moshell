use crate::types::hir::TypeId;
use crate::types::ty::Type;
use std::collections::HashMap;

pub(self) mod builtin;
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
    implicits: HashMap<TypeId, TypeId>,
}

impl Typing {
    /// Constructs a new typing context that already contains the built-in types.
    pub fn with_lang() -> Self {
        Self {
            types: vec![
                Type::Error,
                Type::Nothing,
                Type::Bool,
                Type::ExitCode,
                Type::Int,
                Type::Float,
                Type::String,
            ],
            implicits: HashMap::from([(EXIT_CODE, BOOL), (INT, FLOAT)]),
        }
    }

    /// Unifies two type identifiers, returning the type that the right hand side was unified to.
    ///
    /// Unification is successful when the assignation type is a superset of the rvalue type, i.e
    /// when the assignation type is a parent conceptually or technically of the rvalue type.
    /// It is not reflexive, i.e. `unify(a, b)` is not the same as `unify(b, a)`.
    ///
    /// A conversion may be not as simple as a reinterpretation of the value, and may require
    /// a conversion function to be called. Use [`crate::steps::typing::convert_expression`] to
    /// generate the conversion code for a typed expression.
    pub(crate) fn convert_description(
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

    /// Unifies multiple type identifiers in any direction.
    pub fn convert_many(
        &mut self,
        types: impl Iterator<Item = TypeId>,
    ) -> Result<TypeId, UnificationError> {
        let mut types = types.peekable();
        let first = types.next().unwrap();
        types.try_fold(first, |acc, ty| {
            self.convert_description(acc, ty)
                .or_else(|_| self.convert_description(ty, acc))
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
pub const EXIT_CODE: TypeId = TypeId(3);
pub const INT: TypeId = TypeId(4);
pub const FLOAT: TypeId = TypeId(5);
pub const STRING: TypeId = TypeId(6);

/// An error that occurs when two types are not compatible.
#[derive(Debug, PartialEq)]
pub struct UnificationError();
