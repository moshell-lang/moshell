use indexmap::IndexSet;

/// Contains the constants defined in a module constant pool
#[derive(Default)]
pub struct ConstantPool {
    pub strings: IndexSet<String>,
}

impl ConstantPool {
    /// inserts if not already contained a string in the constant pool, returning it's pool identifier
    pub fn insert_string(&mut self, str: impl ToString) -> u32 {
        let constant_index = self.strings.insert_full(str.to_string()).0;
        u32::try_from(constant_index).expect("constant pool exceeded max capacity")
    }
}
