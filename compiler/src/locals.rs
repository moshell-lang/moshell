use analyzer::typing::user::TypeId;
use analyzer::typing::variable::{LocalEnvironment, LocalId};

use crate::r#type::ValueStackSize;

/// contains the different index per local value allocated in the locals area
pub struct LocalsLayout {
    /// the start indexes of bound Locals
    values_indexes: Vec<(u32, bool)>,

    /// the length in bytes
    len: u32,
}

impl LocalsLayout {
    pub fn new(locals: &LocalEnvironment) -> Self {
        let mut next_offset = 0u32;
        Self {
            values_indexes: locals
                .locals
                .iter()
                .map(|local| {
                    let size = ValueStackSize::from(local.ty) as u8;
                    let index = (next_offset, local.ty.is_obj());
                    next_offset += size as u32;
                    index
                })
                .collect::<Vec<_>>(),
            len: next_offset,
        }
    }

    /// Creates a new local and reserves the space for it.
    pub fn push_value_space(&mut self, tpe: TypeId) -> LocalId {
        let id = LocalId(self.values_indexes.len());
        let size = ValueStackSize::from(tpe) as u8;
        self.values_indexes.push((self.len, tpe.is_obj()));
        self.len += size as u32;
        id
    }

    /// Get the starting byte index allocated for the given local.
    /// and a flag specifying if the local refers to an object reference.
    ///
    /// # Panics
    /// Panics if the local id is out of bounds.
    pub fn get_index(&self, id: LocalId) -> u32 {
        self.values_indexes[id.0].0
    }

    pub fn refs_offset(self) -> Vec<u32> {
        self.values_indexes
            .into_iter()
            .filter_map(|(pos, is_obj)| is_obj.then_some(pos))
            .collect()
    }

    pub fn get_capture_index(&self, var: LocalId) -> Option<u32> {
        None
    }

    pub fn byte_count(&self) -> u32 {
        self.len
    }
}
