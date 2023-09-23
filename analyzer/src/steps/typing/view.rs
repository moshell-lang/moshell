use crate::relations::Definition;
use crate::steps::typing::exploration::Exploration;
use crate::types::ty::{Type, TypeRef};
use std::fmt;

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
        write!(f, "{}", self)
    }
}

impl fmt::Display for TypeInstance<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.exploration.get_type(self.id).unwrap_or(&Type::Error) {
            Type::Error => write!(f, "Error"),
            Type::Unknown => write!(f, "Unknown"),
            Type::Nothing => write!(f, "Nothing"),
            Type::Unit => write!(f, "Unit"),
            Type::Bool => write!(f, "Bool"),
            Type::ExitCode => write!(f, "Exitcode"),
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::String => write!(f, "String"),
            Type::Function(id) => write!(
                f,
                "fun#{}",
                match id {
                    Definition::User(id) => id.0,
                    Definition::Native(id) => id.0,
                }
            ),
            Type::Vector => write!(f, "Vec"),
            Type::Option => write!(f, "Option"),
            Type::Polytype => write!(f, "T"),
            Type::Instantiated(def, parameters) => {
                write!(f, "{}[", TypeInstance::new(*def, self.exploration))?;
                for (i, parameter) in parameters.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", TypeInstance::new(*parameter, self.exploration))?;
                }
                write!(f, "]")
            }
        }
    }
}
