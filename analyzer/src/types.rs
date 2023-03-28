use std::ops::Deref;
use crate::builtin_types::unit;
use std::fmt::{Debug, Display, Formatter, Pointer, Write, write};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ParametrizedType {
    pub name: String,
    pub params: Vec<Type>,
}

impl ParametrizedType {
    pub fn cons(name: &str) -> Self {
        Self::parametrized(name, Vec::new())
    }

    pub fn parametrized(name: &str, params: Vec<Type>) -> Self {
        Self {
            name: name.to_owned(),
            params,
        }
    }
}

impl Display for ParametrizedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.params.is_empty() {
            display_type_list('[', ']', &self.params, f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CallableType {
    pub inputs: Vec<Type>,
    pub output: Box<Type>,
}

impl Display for CallableType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        display_type_list('(', ')', &self.inputs, f)?;
        write!(f, "=>")?;
        write!(f, "{}", self.output.as_ref())
    }
}

/// Represents [monotypes][1] (fully instantiated, unquantified types).
///
/// [1]: https://en.wikipedia.org/wiki/Hindley–Milner_type_system#Monotypes
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    ///A Defined, structured type
    Defined(DefinedType),

    ///Special handling for nothing types
    Nothing,
    ///The type isn't known yet
    Unknown,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DefinedType {
    /// parametrized or constant types (`List[A]`, `List[Str]`, `Int`).
    Parametrized(ParametrizedType),

    ///Type for callables, functions or lambdas, with inputs and output
    Callable(CallableType)
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Defined(d) => write!(f, "{}", d),
            Type::Nothing => write!(f, "Nothing"),
            Type::Unknown => write!(f, "<unknown>"),
        }
    }
}

impl Display for DefinedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DefinedType::Parametrized(p) => write!(f, "{}", p),
            DefinedType::Callable(c) => write!(f, "{}", c)
        }
    }
}

impl<'a> From<ast::r#type::Type<'a>> for Type {
    fn from(value: ast::r#type::Type<'a>) -> Self {
        match value {
            ast::r#type::Type::Simple(s) => Type::Defined(DefinedType::Parametrized(
                ParametrizedType::parametrized(
                    s.name,
                    s.params.into_iter().map(|t| t.into()).collect(),
                )
            )),
            ast::r#type::Type::Callable(c) => Type::Defined(DefinedType::Callable(CallableType {
                inputs: c.params.into_iter().map(|t| t.into()).collect(),
                output: Box::new(c.output.deref().clone().into()),
            })),
            ast::r#type::Type::ByName(b) => Type::Defined(DefinedType::Callable(CallableType {
                inputs: Vec::new(),
                output: Box::new(b.name.deref().clone().into()),
            })),
            ast::r#type::Type::Unit => Type::Defined(unit()),
            ast::r#type::Type::Nothing => Type::Nothing
        }
    }
}



fn display_type_list<'a>(
    start: char,
    end: char,
    types: &Vec<Type>,
    f: &mut Formatter<'_>,
) -> std::fmt::Result {
    f.write_char(start)?;
    if let Some((first, rest)) = types.split_first() {
        write!(f, "{first}")?;
        for ty in rest {
            write!(f, ", {ty}")?;
        }
    }
    f.write_char(end)
}
