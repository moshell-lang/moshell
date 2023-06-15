use indexmap::IndexSet;

/// Contains the constants defined in constant pool
#[derive(Default)]
pub struct ConstantPool {
    pub strings: IndexSet<String>,
}

impl ConstantPool {
    /// inserts if not already contained a string in the constant pool, returning it's pool identifier
    pub fn insert_string(&mut self, str: String) -> usize {
        self.strings.insert_full(str).0
    }
}
