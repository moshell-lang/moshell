use crate::types::hir::TypeId;
use std::fmt::Display;

/// An internal type representation.
#[derive(Clone, Debug, Default, PartialEq)]
pub enum Type {
    /// Reports a previous type error that is propagated.
    Error,

    /// A type that have not been inferred yet.
    #[default]
    Unknown,

    /// A void type, that contains no value.
    Nothing,

    /// A boolean type, either `true` or `false`.
    Bool,

    /// An integer type, that contains a 32-bit signed integer.
    Int,

    /// A floating point type, that contains a 32-bit floating point number.
    Float,

    /// A string type, that contains a UTF-8 string.
    String,

    /// A callable type, that have a separate environment.
    Function {
        parameters: Vec<TypeId>,
        return_type: TypeId,
    },
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Error => write!(f, "Error"),
            Type::Unknown => write!(f, "Unknown"),
            Type::Nothing => write!(f, "Nothing"),
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::String => write!(f, "String"),
            Type::Function {
                parameters,
                return_type,
            } => {
                write!(f, "fn({:?}) -> {:?}", parameters, return_type)
            }
        }
    }
}
