use crate::types;
use crate::types::ty::TypeRef;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

/// Bind a polytype to largest possible monotype
pub struct TypesBounds {
    bounds: HashMap<TypeRef, TypeRef>,
}

impl TypesBounds {
    pub fn new(base: HashMap<TypeRef, TypeRef>) -> Self {
        Self { bounds: base }
    }

    pub fn inactive() -> Self {
        Self::new(HashMap::new())
    }

    pub fn get_bound(&self, ty: TypeRef) -> TypeRef {
        *self.bounds.get(&ty).unwrap_or(&ty)
    }

    /// update bounds of base only if the new bound is larger and not equal to the base type
    /// returns true if the bound has been modified, false if the base was not registered in this type's bound
    /// or if the new bound is invalid
    pub fn update_bound(&mut self, base: TypeRef, new_bound: TypeRef) -> bool {
        // As there is no real hierarchy, only the Nothing type can be more specific than any other type
        // also cancel if the new bound is the same as the base type
        if new_bound == types::NOTHING || base == new_bound {
            return false;
        }
        match self.bounds.entry(base) {
            Entry::Occupied(mut o) => {
                o.insert(new_bound);
                true
            }
            Entry::Vacant(_) => false,
        }
    }
}
