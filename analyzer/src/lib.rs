#![allow(dead_code)]

use std::collections::HashSet;

use crate::diagnostic::Diagnostic;
use crate::engine::Engine;
use crate::importer::ASTImporter;
use crate::name::Name;
use crate::relations::Relations;
use crate::steps::collect::SymbolCollector;
use crate::steps::resolve::SymbolResolver;

pub mod diagnostic;
pub mod engine;
pub mod environment;
pub mod importer;
pub mod name;
pub mod relations;

pub mod steps;

pub fn analyze<'a>(entry_point: Name, importer: &mut impl ASTImporter<'a>) -> AnalyzerOutput<'a> {
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

    AnalyzerOutput {
        engine,
        relations,
        diagnostics,
    }
}

pub struct AnalyzerOutput<'e> {
    pub engine: Engine<'e>,
    pub relations: Relations,
    pub diagnostics: Vec<Diagnostic>,
}
