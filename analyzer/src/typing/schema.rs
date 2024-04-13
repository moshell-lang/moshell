use crate::typing::registry::{FunctionId, Registry};
use crate::typing::{Parameter, TypeId};
use std::collections::HashMap;

/// A structure definition, describing a type with fields and methods.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Schema {
    /// The display name of the schema.
    pub name: String,

    /// The [`crate::typing::user::UserType::GenericVariable`]s used.
    pub generic_variables: Vec<TypeId>,

    /// The fields and their types.
    pub fields: HashMap<String, Parameter>,

    /// The methods and their types.
    pub methods: HashMap<String, FunctionId>,
}

impl Schema {
    /// Creates a new schema.
    pub fn new(name: String) -> Self {
        Self {
            name,
            generic_variables: Vec::new(),
            fields: HashMap::new(),
            methods: HashMap::new(),
        }
    }

    /// Creates a new generic schema.
    pub fn generic(name: String, generic_variables: Vec<TypeId>) -> Self {
        Self {
            name,
            generic_variables,
            fields: HashMap::new(),
            methods: HashMap::new(),
        }
    }

    pub fn get_exact_method(
        &self,
        registry: &Registry,
        name: &str,
        params: &[TypeId],
        return_ty: TypeId,
    ) -> Option<FunctionId> {
        self.methods.get(name).and_then(|&id| {
            let func = &registry[id];
            if func
                .param_types
                .iter()
                .map(|param| param.ty)
                .eq(params.iter().copied())
                && func.return_type == return_ty
            {
                Some(id)
            } else {
                None
            }
        })
    }
}
