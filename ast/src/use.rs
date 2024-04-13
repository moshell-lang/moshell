use crate::variable::Identifier;
use context::source::{SourceSegment, SourceSegmentHolder};
use src_macros::segment_holder;
use std::fmt::{Display, Formatter};

/// The expression that imports an external symbol into its current scope
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct Use {
    /// all the used variables/functions, types, environment variable names
    pub import: Import,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InclusionPathItem {
    Symbol(Identifier),
    Reef(SourceSegment),
}

impl InclusionPathItem {
    pub fn name(&self) -> &str {
        match self {
            InclusionPathItem::Symbol(ident) => ident.value.as_str(),
            InclusionPathItem::Reef(_) => "reef",
        }
    }
}

impl SourceSegmentHolder for InclusionPathItem {
    fn segment(&self) -> SourceSegment {
        match self {
            InclusionPathItem::Symbol(ident) => ident.segment(),
            InclusionPathItem::Reef(s) => s.clone(),
        }
    }
}

impl Display for InclusionPathItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InclusionPathItem::Symbol(ident) => write!(f, "{ident}"),
            InclusionPathItem::Reef(_) => write!(f, "reef"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Import {
    ///A symbol (or list of symbols)
    Symbol(ImportedSymbol),
    /// all in given module (the vec being the inclusion path where the last element is the module that is being imported)
    AllIn(Vec<InclusionPathItem>, SourceSegment),
    ///An environment variable, command.
    Environment(Identifier),
    ///An import list
    List(ImportList),
}

impl SourceSegmentHolder for Import {
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
#[derive(Debug, Clone, PartialEq)]
pub struct ImportList {
    ///list of relative prefixed modules
    pub root: Vec<InclusionPathItem>,

    ///All the imports
    pub imports: Vec<Import>,
}

///An imported symbol. can be a constant, function, type or a module.
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct ImportedSymbol {
    ///list of relative prefixed modules, with the symbol's name
    pub path: Vec<InclusionPathItem>,

    ///The symbol's alias (if any)
    pub alias: Option<Identifier>,
}
