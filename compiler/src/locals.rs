use analyzer::relations::LocalId;

use crate::r#type::ValueStackSize;

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

    /// Reserves the space in the locals depending on the stack size needed by the given type.
    ///
    /// Different initialization orders will result in different indexes.
    ///
    /// # Panics
    /// Panics if the local id is out of bounds.
    pub fn set_space(&mut self, id: LocalId, stack_size: ValueStackSize) {
        let size: u8 = stack_size.into();
        self.indexes[id.0] = Some(self.len);
        self.len += size as u32;
    }

    /// Get the starting byte index allocated for the given local.
    ///
    /// Returns [`None`] if the local is size has not yet been initialized.
    ///
    /// # Panics
    /// Panics if te local id is out of bounds.
    pub fn get_index(&self, id: LocalId) -> Option<u32> {
        self.indexes[id.0]
    }

    pub fn byte_count(&self) -> u32 {
        self.len
    }
}
