use dbg_pls::DebugPls;

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Type<'a> {
    Monotype(Monotype<'a>),
    Polytype(Polytype<'a>)
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
