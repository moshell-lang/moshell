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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    Function,
    Constructor,
    Intrinsic,
}

impl Function {
    pub fn native(
        fqn: &'static str,
        generic_variables: Vec<TypeId>,
        param_types: Vec<TypeId>,
        return_type: TypeId,
    ) -> Self {
        Self {
            declared_at: PathBuf::new(),
            fqn: PathBuf::from(fqn),
            generic_variables,
            param_types: param_types
                .into_iter()
                .map(|ty| Parameter {
                    ty,
                    span: Default::default(),
                })
                .collect(),
            return_type,
            kind: FunctionKind::Intrinsic,
        }
    }
}
