use context::source::{SourceSegment, SourceSegmentHolder};
use src_macros::segment_holder;

///a `use x, y, z` expression
#[segment_holder]
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
    AllIn(Vec<&'a str>, SourceSegment),
    ///An environment variable, command.
    Environment(&'a str, SourceSegment),

    List(ImportList<'a>),
}

impl<'a> SourceSegmentHolder for Import<'a> {
    fn segment(&self) -> SourceSegment {
        match self {
            Import::Symbol(s) => s.segment.clone(),
            Import::AllIn(_, s) => s.clone(),
            Import::Environment(_, s) => s.clone(),
            Import::List(l) => l.segment.clone(),
        }
    }
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq, dbg_pls::DebugPls)]
pub struct ImportList<'a> {
    ///list of relative prefixed modules
    pub path: Vec<&'a str>,

    ///All the imports
    pub imports: Vec<Import<'a>>,
}

///An imported symbol. can be a constant, function, type or a module.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, dbg_pls::DebugPls)]
pub struct ImportedSymbol<'a> {
    ///list of relative prefixed modules
    pub path: Vec<&'a str>,

    ///The symbol's type
    pub name: &'a str,

    ///The symbol's alias (if any)
    pub alias: Option<&'a str>,
}
