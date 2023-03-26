use crate::types::TypeScheme;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct ClassType {
    pub args: Vec<TypeScheme>,
    pub callable: Option<CallableType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallableType {
    pub args: Vec<TypeScheme>,
    pub return_type: TypeScheme,
}
