use crate::Expr;
use context::display::fmt_comma_separated;
use dbg_pls::DebugPls;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Type<'a> {
    ///A Simple type with optional parameters (`A`, `A[V]`)
    Simple(SimpleType<'a>),

    ///A callable declaration with of form `(A, B, ...) => Out`
    Callable(CallableType<'a>),

    ///A By name declaration (`=> X`)
    ByName(ByName<'a>),

    ///Either `()` or `Unit`, representing a void type
    Unit,

    ///The Nothing types
    Nothing,
}

///a casted expression
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct CastedExpr<'a> {
    ///the underlying expression
    pub expr: Box<Expr<'a>>,

    ///the casted type
    pub casted_type: Type<'a>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct SimpleType<'a> {
    pub name: &'a str,
    pub params: Vec<Type<'a>>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct ByName<'a> {
    pub name: Box<Type<'a>>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct CallableType<'a> {
    pub params: Vec<Type<'a>>,
    pub output: Box<Type<'a>>,
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Simple(m) => Display::fmt(m, f),
            Type::Callable(p) => Display::fmt(p, f),
            Type::ByName(n) => Display::fmt(n, f),
            Type::Unit => write!(f, "Unit"),
            Type::Nothing => write!(f, "Nothing"),
        }
    }
}

impl<'a> Display for CallableType<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let inputs = &self.params;
        if let Some(Type::Simple(first_in)) = inputs.first() {
            Display::fmt(first_in, f)?;
        } else {
            fmt_comma_separated('(', ')', inputs, f)?;
        }
        f.write_str(" => ")?;
        Display::fmt(self.output.deref(), f)
    }
}

impl<'a> Display for SimpleType<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)?;
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
