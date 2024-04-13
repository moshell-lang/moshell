use crate::r#type::ValueStackSize;
use analyzer::typing::schema::Schema;
use analyzer::typing::variable::LocalId;

#[derive(Debug, Clone, PartialEq)]
pub struct StructureLayout {
    field_offset: usize,
    pub(crate) total_size: u32,
    indexes: Vec<(u32, ValueStackSize)>,
}

impl From<&Schema> for StructureLayout {
    fn from(structure: &Schema) -> Self {
        let mut indexes = Vec::new();
        let mut idx = 0;

        for field in structure.fields.values() {
            let field_size = ValueStackSize::from(field.ty);
            indexes.push((idx, field_size));
            idx += u8::from(field_size) as u32;
        }

        Self {
            field_offset: structure.generic_variables.len(),
            total_size: idx,
            indexes,
        }
    }
}

impl StructureLayout {
    pub fn get_emplacement(&self, field: LocalId) -> (u32, ValueStackSize) {
        self.indexes[field.0 - self.field_offset]
    }
}
