use analyzer::relations::LocalId;
use analyzer::types::ty::StructureDesc;

use crate::r#type::ValueStackSize;

#[derive(Debug, Clone, PartialEq)]
pub struct StructureLayout {
    field_offset: usize,
    pub(crate) total_size: u32,
    indexes: Vec<(u32, ValueStackSize)>,
}

impl From<&StructureDesc> for StructureLayout {
    fn from(structure: &StructureDesc) -> Self {
        let mut indexes = Vec::new();
        let mut idx = 0;

        for field in structure.get_fields() {
            let field_size = ValueStackSize::from(field.ty);
            indexes.push((idx, field_size));
            idx += u8::from(field_size) as u32;
        }

        Self {
            field_offset: structure.type_parameters.len(),
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
