use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

use context::display::fmt_comma_separated;
use context::source::{SourceSegment, SourceSegmentHolder};
use src_macros::segment_holder;

use crate::r#use::InclusionPathItem;
use crate::variable::Identifier;
use crate::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    ///A Simple type with optional parameters (`A`, `A[V]`, std::foo::Option[A])
    Parametrized(ParametrizedType),

    ///A callable declaration with of form `(A, B, ...) => Out`
    Callable(CallableType),

    ///A By name declaration (`=> X`)
    ByName(ByName),
}

/// A casted expression
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct CastedExpr {
    ///the underlying expression
    pub expr: Box<Expr>,

    ///the casted type
    pub casted_type: Type,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParameter {
    pub name: Identifier,
    pub params: Vec<TypeParameter>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct ParametrizedType {
    /// inclusion path, with the type's name
    pub path: Vec<InclusionPathItem>,

    ///the type's parameters
    pub params: Vec<Type>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct ByName {
    pub name: Box<Type>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct CallableType {
    pub params: Vec<Type>,
    pub output: Box<Type>,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Parametrized(m) => Display::fmt(m, f),
            Type::Callable(p) => Display::fmt(p, f),
            Type::ByName(n) => Display::fmt(n, f),
        }
    }
}

impl SourceSegmentHolder for Type {
    fn segment(&self) -> SourceSegment {
        match self {
            Type::Parametrized(m) => m.segment(),
            Type::Callable(p) => p.segment(),
            Type::ByName(n) => n.segment(),
        }
    }
}

impl Display for CallableType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let inputs = &self.params;
        if let Some(Type::Parametrized(first_in)) = inputs.first() {
            Display::fmt(first_in, f)?;
        } else {
            fmt_comma_separated('(', ')', inputs, f)?;
        }
        f.write_str(" => ")?;
        Display::fmt(self.output.deref(), f)
    }
}

impl Display for ParametrizedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some((first, tail)) = self.path.split_first() {
            write!(f, "{first}")?;
            for it in tail {
                write!(f, "::{it}")?;
            }
        }
        if self.params.is_empty() {
            return Ok(());
        }
        fmt_comma_separated('[', ']', &self.params, f)
    }
}

impl Display for ByName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("=> ")?;
        Display::fmt(&self.name, f)
    }
}
