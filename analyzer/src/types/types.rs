use std::fmt::{Debug, Display, Formatter, Write};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ParameterizedType {
    pub name: String,
    pub params: Vec<Type>,
}

impl ParameterizedType {
    pub fn cons(name: &str) -> Self {
        Self::parametrized(name, &[])
    }

    pub fn parametrized(name: &str, params: &[Type]) -> Self {
        Self {
            name: name.to_owned(),
            params: params.to_vec(),
        }
    }
}

impl Display for ParameterizedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.params.is_empty() {
            display_type_list('[', ']', &self.params, f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DefinedType {
    /// parametrized or constant types (`List[A]`, `Map[Str, List[B]]`, `Int`).
    Parameterized(ParameterizedType),
    //Callable ?
}

impl DefinedType {
    pub fn cons(name: &str) -> Self {
        Self::parametrized(name, &[])
    }

    pub fn parametrized(name: &str, params: &[Type]) -> Self {
        DefinedType::Parameterized(ParameterizedType::parametrized(name, params))
    }
}

/// Represents [monotypes][1] (fully instantiated, unquantified types).
///
/// [1]: https://en.wikipedia.org/wiki/Hindley–Milner_type_system#Monotypes
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    ///A Defined, structured types
    Defined(DefinedType),

    ///Special handling for nothing types
    Nothing,

    ///The types isn't known yet
    Unknown,
}

impl Type {
    pub fn cons(name: &str) -> Self {
        Self::parametrized(name, &[])
    }

    pub fn parametrized(name: &str, params: &[Type]) -> Self {
        Type::Defined(DefinedType::parametrized(name, params))
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Defined(d) => write!(f, "{}", d),
            Type::Unknown => write!(f, "<unknown>"),
            Type::Nothing => write!(f, "Nothing"),
        }
    }
}

impl Display for DefinedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DefinedType::Parameterized(p) => write!(f, "{}", p),
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
