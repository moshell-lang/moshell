///a `use x, y, z` expression
#[derive(Debug, Clone, PartialEq, dbg_pls::DebugPls)]
pub struct Use<'a> {
    ///all the used variables/functions, types, environment variable names
    pub import: Import<'a>,
}

#[derive(Debug, Clone, PartialEq, dbg_pls::DebugPls)]
pub enum Import<'a> {
    ///A symbol (or list of symbols)
    Symbol(ImportedSymbol<'a>),
    /// all in given module (the vec being the module chain where the last is the used module)
    AllIn(Vec<&'a str>),
    ///An environment variable, command.
    Environment(&'a str),

    List(ImportList<'a>),
}

#[derive(Debug, Clone, PartialEq, dbg_pls::DebugPls)]
pub struct ImportList<'a> {
    ///list of prefixed modules (with an optional file path at index 0 of the vec)
    pub path: Vec<&'a str>,

    ///All the imports
    pub imports: Vec<Import<'a>>,
}

///An imported symbol. can be a constant, function, type or a module.
#[derive(Debug, Clone, PartialEq, dbg_pls::DebugPls)]
pub struct ImportedSymbol<'a> {
    ///list of prefixed modules
    pub path: Vec<&'a str>,

    ///The symbol's type
    pub name: &'a str,

    ///The symbol's alias (if any)
    pub alias: Option<&'a str>,
}
