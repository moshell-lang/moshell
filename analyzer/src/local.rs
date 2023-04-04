use crate::types::types::Type;

/// A local variable.
#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    /// The name of the variable.
    pub name: String,

    /// The type of the variable.
    pub ty: Type,

    /// Whether the variable has been initialized.
    pub is_initialized: bool,
}
