use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

use dbg_pls::DebugPls;

use context::display::fmt_comma_separated;
use context::source::{SourceSegment, SourceSegmentHolder};
use src_macros::segment_holder;

use crate::r#use::InclusionPathItem;
use crate::Expr;

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Type<'a> {
    ///A Simple type with optional parameters (`A`, `A[V]`, std::foo::Option[A])
    Parametrized(ParametrizedType<'a>),

    ///A callable declaration with of form `(A, B, ...) => Out`
    Callable(CallableType<'a>),

    ///A By name declaration (`=> X`)
    ByName(ByName<'a>),
}

/// A casted expression
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct CastedExpr<'a> {
    ///the underlying expression
    pub expr: Box<Expr<'a>>,

    ///the casted type
    pub casted_type: Type<'a>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct ParametrizedType<'a> {
    /// inclusion path, with the type's name
    pub path: Vec<InclusionPathItem<'a>>,

    ///the type's parameters
    pub params: Vec<Type<'a>>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct ByName<'a> {
    pub name: Box<Type<'a>>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct CallableType<'a> {
    pub params: Vec<Type<'a>>,
    pub output: Box<Type<'a>>,
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Parametrized(m) => Display::fmt(m, f),
            Type::Callable(p) => Display::fmt(p, f),
            Type::ByName(n) => Display::fmt(n, f),
        }
    }
}

impl SourceSegmentHolder for Type<'_> {
    fn segment(&self) -> SourceSegment {
        match self {
            Type::Parametrized(m) => m.segment(),
            Type::Callable(p) => p.segment(),
            Type::ByName(n) => n.segment(),
        }
    }
}

impl<'a> Display for CallableType<'a> {
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

impl<'a> Display for ParametrizedType<'a> {
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

impl<'a> Display for ByName<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("=> ")?;
        Display::fmt(&self.name, f)
    }
}
