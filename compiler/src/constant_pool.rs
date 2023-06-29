use std::fmt::{Display, Formatter};

use crate::r#type::type_to_bytecode_str;
use analyzer::name::Name;
use analyzer::types::ty::Type;
use indexmap::IndexSet;

/// Contains the constants defined in a module constant pool
#[derive(Default)]
pub struct ConstantPool {
    pub strings: IndexSet<String>,
}

impl ConstantPool {
    /// inserts if not already contained a string in the constant pool, returning it's pool identifier
    pub fn insert_string(&mut self, str: &str) -> u32 {
        self.insert(str.to_string())
    }

    /// inserts if not already contained a function signature in the constant pool, returning it's pool identifier
    pub fn insert_signature(&mut self, signature: FunctionSignature) -> u32 {
        self.insert(signature.to_string())
    }

    fn insert(&mut self, str: String) -> u32 {
        u32::try_from(self.strings.insert_full(str).0).expect("constant pool exceeded max capacity")
    }
}

/// Function signature where all the strings are
/// u32 indexes in the bound constant pool
pub struct FunctionSignature {
    pub full_name: Name,
    pub params: Vec<Type>,
    pub return_type: Type,
}

impl FunctionSignature {
    pub fn new(full_name: Name, params: Vec<Type>, return_type: Type) -> Self {
        Self {
            full_name,
            params,
            return_type,
        }
    }
}

impl Display for FunctionSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.full_name)?;

        if let Some((head, tail)) = self.params.split_first() {
            write!(f, "{}", type_to_bytecode_str(head))?;
            for it in tail {
                write!(f, ";{}", type_to_bytecode_str(it))?;
            }
        }

        write!(f, "){}", type_to_bytecode_str(&self.return_type))
    }
}
