#![allow(dead_code)]

use std::collections::HashSet;

use crate::diagnostic::Diagnostic;
use crate::engine::Engine;
use crate::importer::ASTImporter;
use crate::imports::Imports;
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

pub mod imports;
pub mod steps;

/// Performs a full resolution of the environments directly or indirectly implied by the entry point.
///
/// The completion of a collection followed by its resolution phase is called a cycle.
/// Multiple cycles can occur if the resolution phase finds new modules to collect.
pub fn analyze_all<'a>(
    entry_point: Name,
    importer: &mut impl ASTImporter<'a>,
) -> ResolutionResult<'a> {
    let mut engine = Engine::default();
    let mut relations = Relations::default();
    let mut imports = Imports::default();

    let mut to_visit = vec![entry_point];
    let mut visited = HashSet::new();

    let mut diagnostics = Vec::new();
    while !to_visit.is_empty() {
        diagnostics.extend(SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &mut to_visit,
            &mut visited,
            importer,
        ));
        diagnostics.extend(SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
            &mut to_visit,
            &mut visited,
        ));
        // The cycle ended, if `to_visit` is still non empty, a new cycle will be started
        // to resolve the modules to visit and so on
    }

    ResolutionResult {
        engine,
        relations,
        diagnostics,
    }
}

/// Performs a full resolution of the environments directly or indirectly implied by the entry point.
///
/// The completion of a collection followed by its resolution phase is called a cycle.
/// Multiple cycles can occur if the resolution phase finds new modules to collect.
pub fn make_full_resolution<'a, 'e>(
    entry_point: Name,
    importer: &mut impl ASTImporter<'e>,
    engine: &'a mut Engine<'e>,
    relations: &'a mut Relations,
    imports: &'a mut Imports,
) -> Vec<Diagnostic> {
    let mut to_visit = vec![entry_point];
    let mut visited = HashSet::new();

    let mut diagnostics = Vec::new();
    while !to_visit.is_empty() {
        diagnostics.extend(SymbolCollector::collect_symbols(
            engine,
            relations,
            imports,
            &mut to_visit,
            &mut visited,
            importer,
        ));
        diagnostics.extend(SymbolResolver::resolve_symbols(
            engine,
            relations,
            imports,
            &mut to_visit,
            &mut visited,
        ));
        // The cycle ended, if `to_visit` is still non empty, a new cycle will be started
        // to resolve the modules to visit and so on
    }

    diagnostics
}

/// The results of an analysis
pub struct ResolutionResult<'e> {
    pub engine: Engine<'e>,
    pub relations: Relations,
    pub diagnostics: Vec<Diagnostic>,
}
