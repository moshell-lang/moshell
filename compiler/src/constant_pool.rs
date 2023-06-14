use indexmap::IndexSet;

#[derive(Default)]
pub struct ConstantPool {
    pub strings: IndexSet<String>,
}

impl ConstantPool {
    pub fn insert_string(&mut self, str: String) -> usize {
        self.strings.insert_full(str).0
    }
}