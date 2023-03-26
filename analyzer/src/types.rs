use std::ops::Deref;
use crate::builtin_types::unit;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DefinedType {
    pub name: String,
    pub params: Vec<Type>,
}

impl DefinedType {
    pub fn cons(name: &str) -> Self {
        Self::parametrized(name, Vec::new())
    }

    pub fn parametrized(name: &str, params: Vec<Type>) -> Self {
        Self {
            name: name.to_owned(),
            params
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CallableType {
    pub inputs: Vec<Type>,
    pub output: Box<Type>,
}


/// Represents [monotypes][1] (fully instantiated, unquantified types).
///
/// [1]: https://en.wikipedia.org/wiki/Hindley–Milner_type_system#Monotypes
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    /// Constant types.
    Defined(DefinedType),
    Callable(CallableType),

    ///Special handling for nothing types
    Nothing,
    ///The type isn't known yet
    Unknown,
}


impl<'a> From<ast::r#type::Type<'a>> for Type {
    fn from(value: ast::r#type::Type<'a>) -> Self {
        match value {
            ast::r#type::Type::Simple(s) => Type::Defined(
                DefinedType::parametrized(
                    s.name,
                    s.params.into_iter().map(|t| t.into()).collect()
                )
            ),
            ast::r#type::Type::Callable(c) => Type::Callable(CallableType {
                inputs: c.params.into_iter().map(|t| t.into()).collect(),
                output: Box::new(c.output.deref().clone().into()),
            }),
            ast::r#type::Type::ByName(b) => Type::Callable(CallableType {
                inputs: Vec::new(),
                output: Box::new(b.name.deref().clone().into()),
            }),
            ast::r#type::Type::Unit => Type::Defined(unit()),
            ast::r#type::Type::Nothing => Type::Nothing
        }
    }
}