use crate::types::class::TypeClass;

pub struct Local {
    pub name: String,
    ty: Option<TypeClass>,
    is_val: bool,
}
