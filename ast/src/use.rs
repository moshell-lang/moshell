use context::source::{SourceSegment, SourceSegmentHolder};
use dbg_pls::DebugPls;
use src_macros::segment_holder;

/// The expression that imports an external symbol into its current scope
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Use<'a> {
    /// all the used variables/functions, types, environment variable names
    pub import: Import<'a>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct InclusionPath<'a> {
    /// set to true if the import explicitly refers to the current reef
    pub in_reef_explicit: bool,

    pub items: Vec<&'a str>,
}

impl InclusionPath<'_> {
    pub fn empty(segment: SourceSegment) -> Self {
        Self {
            in_reef_explicit: false,
            items: vec![],
            segment,
        }
    }
}

impl<'a> From<InclusionPath<'a>> for Vec<&'a str> {
    fn from(value: InclusionPath<'a>) -> Self {
        value.items
    }
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Import<'a> {
    ///A symbol (or list of symbols)
    Symbol(ImportedSymbol<'a>),
    /// all in given module (the vec being the inclusion path where the last element is the module that is being imported)
    AllIn(InclusionPath<'a>, SourceSegment),
    ///An environment variable, command.
    Environment(&'a str, SourceSegment),
    ///An import list
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
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct ImportList<'a> {
    ///list of relative prefixed modules
    pub root: Option<InclusionPath<'a>>,

    ///All the imports
    pub imports: Vec<Import<'a>>,
}

///An imported symbol. can be a constant, function, type or a module.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct ImportedSymbol<'a> {
    ///list of relative prefixed modules
    pub path: Option<InclusionPath<'a>>,

    ///The symbol's type
    pub name: &'a str,

    ///The symbol's alias (if any)
    pub alias: Option<&'a str>,
}
