use crate::variable::Identifier;
use context::source::{SourceSegment, SourceSegmentHolder};
use dbg_pls::DebugPls;
use src_macros::segment_holder;
use std::fmt::{Display, Formatter};

/// The expression that imports an external symbol into its current scope
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Use<'a> {
    /// all the used variables/functions, types, environment variable names
    pub import: Import<'a>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum InclusionPathItem<'a> {
    Symbol(Identifier<'a>),
    Reef(SourceSegment),
}

impl InclusionPathItem<'_> {
    pub fn name(&self) -> &str {
        match self {
            InclusionPathItem::Symbol(ident) => ident.value,
            InclusionPathItem::Reef(_) => "reef",
        }
    }
}

impl SourceSegmentHolder for InclusionPathItem<'_> {
    fn segment(&self) -> SourceSegment {
        match self {
            InclusionPathItem::Symbol(ident) => ident.segment(),
            InclusionPathItem::Reef(s) => s.clone(),
        }
    }
}

impl Display for InclusionPathItem<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InclusionPathItem::Symbol(ident) => write!(f, "{ident}"),
            InclusionPathItem::Reef(_) => write!(f, "reef"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Import<'a> {
    ///A symbol (or list of symbols)
    Symbol(ImportedSymbol<'a>),
    /// all in given module (the vec being the inclusion path where the last element is the module that is being imported)
    AllIn(Vec<InclusionPathItem<'a>>, SourceSegment),
    ///An environment variable, command.
    Environment(Identifier<'a>),
    ///An import list
    List(ImportList<'a>),
}

impl<'a> SourceSegmentHolder for Import<'a> {
    fn segment(&self) -> SourceSegment {
        match self {
            Import::Symbol(s) => s.segment.clone(),
            Import::AllIn(_, s) => s.clone(),
            Import::Environment(ident) => ident.segment(),
            Import::List(l) => l.segment.clone(),
        }
    }
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct ImportList<'a> {
    ///list of relative prefixed modules
    pub root: Vec<InclusionPathItem<'a>>,

    ///All the imports
    pub imports: Vec<Import<'a>>,
}

///An imported symbol. can be a constant, function, type or a module.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct ImportedSymbol<'a> {
    ///list of relative prefixed modules, with the symbol's name
    pub path: Vec<InclusionPathItem<'a>>,

    ///The symbol's alias (if any)
    pub alias: Option<Identifier<'a>>,
}
