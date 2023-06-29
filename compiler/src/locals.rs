use crate::r#type::get_type_stack_size;
use analyzer::relations::LocalId;
use analyzer::types::hir::TypeId;
use std::collections::HashMap;

/// contains the different index per local value allocated in the locals area
#[derive(Default)]
pub struct LocalsLayout {
    /// the start indexes of bound Locals
    indexes: HashMap<LocalId, u32>,
    len: u32,
}

impl LocalsLayout {
    /// reserves the space in the locals depending on the stack size needed by the given type
    pub fn expand_layout(&mut self, id: LocalId, ty: TypeId) {
        let size: u8 = get_type_stack_size(ty).into();
        self.indexes.insert(id, self.len);
        self.len += size as u32;
    }

    /// get first index allocated for given local identifier
    pub fn get_index(&self, id: LocalId) -> u32 {
        self.indexes[&id]
    }

    pub fn length(&self) -> u32 {
        self.len
    }
}
