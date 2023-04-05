use crate::r#type::Type;

///a `use x, y, z` expression
#[derive(Debug, Clone, PartialEq, dbg_pls::DebugPls)]
pub struct Use<'a> {
    ///all the used variables/functions, types, environment variable names
    pub uses: Vec<&'a str>,
}

pub enum Import<'a> {
    Type(Type<'a>)
}

pub struct ImportedSymbol<'a> {
    ///list of prefixed modules
    pub location: Vec<&'a str>,

    ///The symbol's type
    pub name: &'a str,

    ///The symbol's alias (if any)
    pub alias: Option<&'a str>,
}