#![allow(dead_code)]

use context::source::SourceSegment;
use std::collections::HashSet;

use crate::diagnostic::Diagnostic;
use crate::engine::Engine;
use crate::importer::ASTImporter;
use crate::name::Name;
use crate::relations::{Relations, SourceObjectId};
use crate::steps::collect::SymbolCollector;
use crate::steps::resolve::SymbolResolver;

pub mod diagnostic;
pub mod engine;
pub mod environment;
pub mod importer;
pub mod name;
pub mod relations;

pub mod steps;

/// Performs a full resolution of the environments directly or indirectly implied by the entry point.
///
/// This function will alternate between collection and resolution phases as long as the resolution
/// phase finds new modules that could be imported.
pub fn resolve_all<'a>(
    entry_point: Name,
    importer: &mut impl ASTImporter<'a>,
) -> ResolutionResult<'a> {
    let mut engine = Engine::default();
    let mut relations = Relations::default();

    let mut to_visit = vec![entry_point];
    let mut visited = HashSet::new();

    let mut diagnostics = Vec::new();
    while !to_visit.is_empty() {
        diagnostics.extend(SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut to_visit,
            &mut visited,
            importer,
        ));
        diagnostics.extend(SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut to_visit,
            &mut visited,
        ));
    }

    ResolutionResult {
        engine,
        relations,
        diagnostics,
    }
}

pub struct ResolutionResult<'e> {
    pub engine: Engine<'e>,
    pub relations: Relations,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(PartialEq, Clone)]
pub struct Span {
    pub source: SourceObjectId,
    pub segment: SourceSegment,
}

impl Span {
    fn new(source: SourceObjectId, segment: SourceSegment) -> Self {
        Self { source, segment }
    }
}
