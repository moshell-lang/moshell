
///a `use x, y, z` expression
#[derive(Debug, Clone, PartialEq, dbg_pls::DebugPls)]
pub struct Use<'a> {
    ///all the used variables/functions, types, environment variable names
    pub uses: Vec<Import<'a>>,
}

#[derive(Debug, Clone, PartialEq, dbg_pls::DebugPls)]
pub enum Import<'a> {
    ///A symbol (or list of symbols)
    Symbols(Vec<ImportedSymbol<'a>>),
    /// all in given module (the vec being the module chain where the last is the used module)
    AllIn(Vec<&'a str>),
    ///An environment variable, command.
    Environment(&'a str)
}

///An imported symbol. can be a constant, function, type or a module.
#[derive(Debug, Clone, PartialEq, dbg_pls::DebugPls)]
pub struct ImportedSymbol<'a> {
    ///list of prefixed modules
    pub location: Vec<&'a str>,

    ///The symbol's type
    pub name: &'a str,

    ///The symbol's alias (if any)
    pub alias: Option<&'a str>,
}