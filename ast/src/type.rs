use std::fmt::{Debug, Display, Formatter, Write};
use std::ops::Deref;
use dbg_pls::DebugPls;

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Type<'a> {
    Monotype(Monotype<'a>),
    Polytype(Polytype<'a>),
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Monotype<'a> {
    pub name: &'a str,
    pub params: Vec<Type<'a>>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Polytype<'a> {
    pub inputs: Vec<Type<'a>>,
    pub output: Box<Type<'a>>,
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Monotype(m) => Display::fmt(m, f),
            Type::Polytype(p) => Display::fmt(p, f)
        }
    }
}

///helper function to write a type list format in a given formatter
fn display_type_list<'a>(start: char, end: char, types: &Vec<Type<'a>>, f: &mut Formatter<'_>) -> std::fmt::Result {
    f.write_char(start)?;
    Display::fmt(types.first().unwrap(), f)?;
    for tpe in &types[1..] {
        Display::fmt(tpe, f)?;
        f.write_str(", ")?;
    }
    f.write_char(end)
}

impl<'a> Display for Polytype<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let inputs = &self.inputs;
        let first_in = inputs.first();
        if inputs.is_empty() || matches!(first_in, Some(Type::Monotype(..))) {
            Display::fmt(first_in.unwrap(), f)?;
        } else {
            display_type_list('(', ')', inputs, f)?;
        }
        f.write_str(" => ")?;
        Display::fmt(self.output.deref(), f)
    }
}


impl<'a> Display for Monotype<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)?;
        if self.params.is_empty() {
            return Ok(())
        }

        display_type_list('[', ']', &self.params, f)
    }
}