use std::fmt;

use crate::relations::Definition;
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
        Type::Error => "Error".to_string(),
        Type::Unknown => "Unknown".to_string(),
        Type::Nothing => "Nothing".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::ExitCode => "Exitcode".to_string(),
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::String => "String".to_string(),
        Type::Function(id) => format!(
            "fun#{}",
            match id {
                Definition::User(id) => id.0,
                Definition::Native(id) => id.0,
            }
        ),
        Type::Vector => "Vec".to_string(),
        Type::Option => "Option".to_string(),
        Type::Polytype => exploration.ctx.get_name(ty.type_id).unwrap().to_string(),
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
    }
    .to_string()
}
