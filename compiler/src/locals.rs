use std::collections::hash_map::Entry;
use std::collections::HashMap;

use analyzer::relations::{LocalId, ResolvedSymbol};
use analyzer::types::hir::Var;
use analyzer::types::ty::TypeRef;

use crate::r#type::ValueStackSize;

/// contains the different index per local value allocated in the locals area
pub struct LocalsLayout {
    /// the start indexes of bound Locals
    values_indexes: Vec<Option<(u32, bool)>>,
    /// the start indexes of bound external values
    external_refs_indexes: HashMap<ResolvedSymbol, u32>,
    /// the length in bytes
    len: u32,
}

impl LocalsLayout {
    pub fn new(var_count: usize) -> Self {
        let var_indexes = vec![None; var_count];
        let external_ref_indexes = HashMap::default();
        Self {
            values_indexes: var_indexes,
            external_refs_indexes: external_ref_indexes,
            len: 0,
        }
    }

    /// Reserves the space in the locals depending on the stack size needed by the given type.
    ///
    /// Different initialization orders will result in different indexes.
    ///
    /// # Panics
    /// Panics if the local id is out of bounds.
    pub fn set_value_space(&mut self, id: LocalId, tpe: TypeRef) {
        let size: u8 = ValueStackSize::from(tpe).into();
        self.values_indexes[id.0] = Some((self.len, tpe.is_obj()));
        self.len += size as u32;
    }

    /// Creates a new local and reserves the space for it.
    pub fn push_value_space(&mut self, tpe: TypeRef) -> LocalId {
        let id = LocalId(self.values_indexes.len());
        let size: u8 = ValueStackSize::from(tpe).into();
        self.values_indexes.push(Some((self.len, tpe.is_obj())));
        self.len += size as u32;
        id
    }

    /// Reserves the space in local's of the external reference, if not already set.
    ///
    /// Different initialization orders will result in different indexes.
    pub fn init_external_ref_space(&mut self, symbol: ResolvedSymbol) {
        match self.external_refs_indexes.entry(symbol) {
            Entry::Occupied(_) => {}
            Entry::Vacant(v) => {
                v.insert(self.len);
                self.len += u8::from(ValueStackSize::QWord) as u32;
            }
        }
    }

    /// Get the starting byte index allocated for the given local.
    /// and a flag specifying if the local refers to an object reference.
    ///
    /// Returns [`None`] if the local is size has not yet been initialized.
    ///
    /// # Panics
    /// Panics if the local id is out of bounds.
    pub fn get_index(&self, id: LocalId) -> Option<u32> {
        self.values_indexes[id.0].map(|(pos, _)| pos)
    }

    pub fn get_var_index(&self, var: Var) -> Option<u32> {
        match var {
            Var::Local(LocalId(id)) => self.values_indexes[id].map(|(pos, _)| pos),
            Var::External(symbol) => self.external_refs_indexes.get(&symbol).copied(),
        }
    }

    pub fn refs_offset(self) -> Vec<u32> {
        self.values_indexes
            .into_iter()
            .filter_map(|val| val.and_then(|(pos, is_obj)| is_obj.then_some(pos)))
            .chain(self.external_refs_indexes.into_values())
            .collect()
    }

    pub fn get_capture_index(&self, var: ResolvedSymbol) -> Option<u32> {
        self.external_refs_indexes.get(&var).copied()
    }

    pub fn byte_count(&self) -> u32 {
        self.len
    }
}
