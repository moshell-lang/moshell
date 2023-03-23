use std::fmt;

/// Represents a [type variable][1] (an unknown type).
///
/// [1]: https://en.wikipedia.org/wiki/Hindley–Milner_type_system#Free_type_variables
pub type Variable = usize;

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

impl Type {
    /// Construct a function type (i.e. `alpha` → `beta`).
    pub fn arrow(alpha: Type, beta: Type) -> Type {
        Type::Constructed("→".to_owned(), vec![alpha, beta])
    }
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

impl fmt::Display for TypeScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            TypeScheme::Monotype(t) => t.fmt(f),
            TypeScheme::Polytype { variable, body } => write!(f, "∀t{}. {}", variable, body),
        }
    }
}
