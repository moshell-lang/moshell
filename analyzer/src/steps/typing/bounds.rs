use crate::steps::typing::exploration::Exploration;
use crate::types;
use crate::types::ty::{Type, TypeRef};
use std::collections::hash_map::Entry;
use std::collections::HashMap;

/// Binds a polytype to largest possible monotype
#[derive(Default)]
pub struct TypesBounds {
    bounds: HashMap<TypeRef, TypeRef>,
}

impl TypesBounds {
    /// Construct a type bounds with no polytype to bound, which makes it inactive
    pub fn inactive() -> Self {
        Self::new(HashMap::new())
    }

    pub fn new(base: HashMap<TypeRef, TypeRef>) -> Self {
        Self { bounds: base }
    }

    pub fn get_bound(&self, ty: TypeRef) -> TypeRef {
        *self.bounds.get(&ty).unwrap_or(&ty)
    }
    /// return true if given type is registered as a bound type but is bound to itself
    pub fn is_self_bound(&self, ty: TypeRef) -> bool {
        self.bounds.get(&ty).is_some_and(|t| *t == ty)
    }

    /// update bounds of registered polytypes from given type scheme correlated with given bounds.
    /// This method will only update bounds that are larger than the current polytypes bounds
    pub(super) fn update_bounds(
        &mut self,
        base: TypeRef,
        new_bounds: TypeRef,
        exploration: &Exploration,
    ) {
        match self.bounds.entry(base) {
            Entry::Occupied(mut o) => {
                // As there is no real hierarchy for now, only the Nothing type can be more specific than any other type
                // if the base type already had a bound (other than himself), we can accept larger types only (thus no NOTHING currently)
                if *o.get() != base && new_bounds == types::NOTHING {
                    return;
                }
                o.insert(new_bounds);
            }
            Entry::Vacant(_) => {
                let base_type = exploration.get_type(base).unwrap();
                let bound_type = exploration.get_type(new_bounds).unwrap();
                if let (Type::Instantiated(b1, p1), Type::Instantiated(b2, p2)) =
                    (base_type, bound_type)
                {
                    for (base, bounds) in p1.iter().zip(p2) {
                        self.update_bounds(*base, *bounds, exploration);
                    }
                    self.update_bounds(*b1, *b2, exploration);
                }
            }
        }
    }
}
