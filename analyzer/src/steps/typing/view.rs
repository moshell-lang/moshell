use std::fmt;

use crate::steps::typing::bounds::TypesBounds;
use crate::steps::typing::exploration::Exploration;
use crate::types::ty::{Type, TypeRef};

#[derive(Copy, Clone)]
pub(super) struct TypeInstance<'a> {
    pub(super) id: TypeRef,
    pub(super) exploration: &'a Exploration<'a>,
}

impl<'a> TypeInstance<'a> {
    pub(super) fn new(id: TypeRef, exploration: &'a Exploration) -> Self {
        Self { id, exploration }
    }
}

impl fmt::Debug for TypeInstance<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "id: {:?}", self.id)
    }
}

pub(super) fn type_to_string(
    ty: TypeRef,
    exploration: &Exploration,
    bounds: &TypesBounds,
) -> String {
    let ty = bounds.get_bound(ty);
    match exploration.get_type(ty).unwrap_or(&Type::Error) {
        Type::Instantiated(def, parameters) => {
            let mut str = type_to_string(*def, exploration, bounds);
            str.push('[');
            for (i, parameter) in parameters.iter().enumerate() {
                if i > 0 {
                    str.push_str(", ");
                }
                str.push_str(&type_to_string(*parameter, exploration, bounds));
            }
            str.push(']');
            str
        }
        Type::Wildcard => "_".to_string(),
        _ => exploration
            .get_type_name(ty)
            .cloned()
            .unwrap_or("<?>".to_string()),
    }
    .to_string()
}
