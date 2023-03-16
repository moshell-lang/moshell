#[derive(Debug, Clone, PartialEq)]
pub struct TypeScheme {
    pub ty: Type,
}

/// All types supported by the language.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    ExitCode,
    Int,
    Float,
    Any,
    Array(Box<Type>),
    Try(Box<Type>),
    Nil,
}

impl From<Type> for TypeScheme {
    fn from(ty: Type) -> Self {
        Self { ty }
    }
}

impl<'a> TryFrom<&'a str> for Type {
    type Error = String;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        match value {
            "bool" => Ok(Type::Bool),
            "exitcode" => Ok(Type::ExitCode),
            "int" => Ok(Type::Int),
            "float" => Ok(Type::Float),
            "any" => Ok(Type::Any),
            "nil" => Ok(Type::Nil),
            _ => Err(format!("Unknown type: {value}")),
        }
    }
}
