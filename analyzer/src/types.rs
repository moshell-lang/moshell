use std::fmt;

/// Represents a [type variable][1] (an unknown type).
///
/// [1]: https://en.wikipedia.org/wiki/Hindley–Milner_type_system#Free_type_variables
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Variable(pub usize);

/// Represents [monotypes][1] (fully instantiated, unquantified types).
///
/// [1]: https://en.wikipedia.org/wiki/Hindley–Milner_type_system#Monotypes
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    /// Primitive or composite types.
    Constructed(String, Vec<Type>),
    /// Type variables.
    Variable(Variable),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Type::Constructed(name, args) => {
                write!(f, "{name}")?;
                if !args.is_empty() {
                    write!(f, "[")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{arg}")?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            Type::Variable(v) => write!(f, "t{v}"),
        }
    }
}

/// Represents [polytypes][1] (uninstantiated, universally quantified types).
///
/// [1]: https://en.wikipedia.org/wiki/Hindley–Milner_type_system#Polytype
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeScheme {
    /// Non-polymorphic types (e.g. `α → β`, `int → bool`)
    Monotype(Type),
    /// Polymorphic types (e.g. `∀α. α → α`, `∀α. ∀β. α → β`)
    Polytype {
        /// The [`Variable`] being bound
        variable: Variable,
        /// The type in which `variable` is bound
        body: Box<TypeScheme>,
    },
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for TypeScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            TypeScheme::Monotype(t) => t.fmt(f),
            TypeScheme::Polytype { variable, body } => write!(f, "∀t{}. {}", variable, body),
        }
    }
}

impl From<&ast::r#type::Type<'_>> for TypeScheme {
    fn from(declared_type: &ast::r#type::Type) -> Self {
        use ast::r#type::Type::*;
        match declared_type {
            Simple(ty) => TypeScheme::Monotype(Type::Constructed(
                ty.name.to_owned(),
                ty.params
                    .iter()
                    .map(|t| match TypeScheme::from(t) {
                        TypeScheme::Monotype(ty) => ty,
                        TypeScheme::Polytype { .. } => todo!("nested type parameters"),
                    })
                    .collect(),
            )),
            Callable(_) => TypeScheme::Monotype(Type::Constructed("→".to_owned(), Vec::new())),
            ByName(_) => TypeScheme::Monotype(Type::Constructed("→".to_owned(), Vec::new())),
            Unit => TypeScheme::Monotype(Type::Constructed("Unit".to_owned(), Vec::new())),
        }
    }
}

impl From<Type> for TypeScheme {
    fn from(ty: Type) -> Self {
        TypeScheme::Monotype(ty)
    }
}
