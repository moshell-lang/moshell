use crate::types::TypeScheme;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct ClassType {
    pub args: Vec<TypeScheme>,
}
