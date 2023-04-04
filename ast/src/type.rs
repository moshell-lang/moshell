use dbg_pls::DebugPls;
use std::fmt::{Debug, Display, Formatter, Write};
use std::ops::Deref;

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Type<'a> {
    ///A Simple type with optional parameters (`A`, `A[V]`)
    Simple(SimpleType<'a>),

    ///A callable declaration with of form `(A, B, ...) => Out`
    Callable(CallableType<'a>),

    ///A By name declaration (`=> X`)
    ByName(ByName<'a>),

    ///Either `()` or `Unit`, representing a void types
    Unit,

    ///The Nothing types
    Nothing,
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

///helper function to write a type list format in a given formatter
fn display_type_list<'a>(
    start: char,
    end: char,
    types: &Vec<Type<'a>>,
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

impl<'a> Display for CallableType<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let inputs = &self.params;
        if let Some(Type::Simple(first_in)) = inputs.first() {
            Display::fmt(first_in, f)?;
        } else {
            display_type_list('(', ')', inputs, f)?;
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

        display_type_list('[', ']', &self.params, f)
    }
}

impl<'a> Display for ByName<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("=> ")?;
        Display::fmt(&self.name, f)
    }
}
