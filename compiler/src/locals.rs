use crate::r#type::get_type_stack_size;
use analyzer::relations::LocalId;
use analyzer::types::hir::TypeId;

/// contains the different index per local value allocated in the locals area
pub struct LocalsLayout {
    /// the start indexes of bound Locals
    indexes: Vec<Option<u32>>,
    /// the length in bytes
    len: u32,
}

impl LocalsLayout {
    pub fn fixed_count(count: usize) -> Self {
        let mut indexes = Vec::with_capacity(count);
        indexes.resize(count, None);
        Self { indexes, len: 0 }
    }

    /// reserves the space in the locals depending on the stack size needed by the given type
    pub fn initialize_space(&mut self, id: LocalId, ty: TypeId) {
        if self.indexes[id.0].is_some() {
            return;
        }
        let size: u8 = get_type_stack_size(ty).into();
        self.indexes[id.0] = Some(self.len);
        self.len += size as u32;
    }

    /// get first index allocated for given local identifier
    pub fn get_index(&self, id: LocalId) -> u32 {
        self.indexes[id.0].unwrap_or_else(|| {
            panic!(
                "Local {} is unknown or has no initialized space in locals layout",
                id.0
            )
        })
    }

    pub fn byte_count(&self) -> u32 {
        self.len
    }
}
