use crate::r#type::get_type_stack_size;
use analyzer::relations::LocalId;
use analyzer::types::hir::TypeId;
use std::collections::HashMap;

#[derive(Default)]
pub struct LocalsLayout {
    sizes: HashMap<LocalId, u32>,
    len: u32,
}

impl LocalsLayout {
    pub fn expand_layout(&mut self, id: LocalId, ty: TypeId) {
        let size: u8 = get_type_stack_size(ty).into();
        self.sizes.insert(id, self.len);
        self.len += size as u32;
    }

    pub fn get_index(&self, id: LocalId) -> u32 {
        self.sizes[&id]
    }

    pub fn length(&self) -> u32 {
        self.len
    }
}
