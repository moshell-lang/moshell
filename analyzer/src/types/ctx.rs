use std::collections::HashMap;

use crate::relations::{LocalId, Relations, SourceId, SymbolRef};
use crate::types::ty::{TypeId, TypeRef};

/// Holds the symbol to type mapping.
///
/// The actual type definition is in the [`crate::types::Typing`] struct.
#[derive(Default, Debug)]
pub struct TypeContext {
    names: HashMap<String, TypeId>,
    locals: HashMap<SourceId, Vec<TypedVariable>>,
}

impl TypeContext {
    /// Returns the type id of a symbol.
    pub(crate) fn get(
        &self,
        relations: &Relations,
        source: SourceId,
        symbol: SymbolRef,
    ) -> Option<TypedVariable> {
        match symbol {
            SymbolRef::Local(local) => self.get_local(source, local),

            SymbolRef::External(index) => {
                let resolved = relations[index].state.expect_resolved("Unresolved symbol");
                // assume that the resolved symbol's reef points to this context's reef
                self.get_local(resolved.source, resolved.object_id)
            }
        }
    }

    pub(crate) fn get_local(&self, source: SourceId, id: LocalId) -> Option<TypedVariable> {
        self.locals.get(&source).unwrap().get(id.0).copied()
    }

    /// Defines the type of a currently explored symbol.
    ///
    /// This must be in sync with the symbol in the environment.
    pub(crate) fn push_local_typed(&mut self, source: SourceId, type_ref: TypeRef) -> LocalId {
        self.push_local(source, TypedVariable::immutable(type_ref))
    }

    /// Defines the identity of a currently explored symbol.
    ///
    /// This must be in sync with the symbol in the environment.
    pub(crate) fn push_local(&mut self, source: SourceId, obj: TypedVariable) -> LocalId {
        let locals = self.locals.entry(source).or_default();
        let index = locals.len();
        locals.push(obj);
        LocalId(index)
    }

    pub(crate) fn bind_name(&mut self, name: String, tpe: TypeId) {
        self.names.insert(name, tpe);
    }

    pub fn get_type_id(&self, name: &str) -> Option<TypeId> {
        self.names.get(name).copied()
    }

    pub fn get_name(&self, ty: TypeId) -> Option<&str> {
        for (name, bound) in &self.names {
            if ty == *bound {
                return Some(name);
            }
        }
        None
    }
}

/// The identity of a variable.
///
/// The main purpose of this struct is to hold the type of a variable,
/// but it also holds if the variable can be reassigned.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct TypedVariable {
    pub(crate) type_ref: TypeRef,
    pub(crate) can_reassign: bool,
}

impl TypedVariable {
    /// Constructs a new mutable variable identity.
    pub(crate) fn assignable(type_ref: TypeRef) -> Self {
        Self {
            type_ref,
            can_reassign: true,
        }
    }

    /// Constructs a new immutable variable identity.
    pub(crate) fn immutable(type_ref: TypeRef) -> Self {
        Self {
            type_ref,
            can_reassign: false,
        }
    }
}
