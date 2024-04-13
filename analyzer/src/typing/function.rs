use crate::typing::{Parameter, TypeId};
use std::path::PathBuf;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub declared_at: PathBuf,
    pub fqn: PathBuf,
    pub generic_variables: Vec<TypeId>,
    pub param_types: Vec<Parameter>,
    pub return_type: TypeId,
    pub kind: FunctionKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FunctionKind {
    Function,
    Constructor,
}
