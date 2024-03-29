//! The Moshell analyzer takes a dynamic set of sources, resolves them and diagnoses
//! errors. It produces at the end a typed intermediate representation of the
//! program that can be directly compiled.
//!
//! The analyzer is composed of a set of steps that are executed in order. Each
//! step is responsible for a specific task, and the analyzer is responsible for
//! orchestrating the steps. Those tasks fail-fast, meaning that if a step fails,
//! the analyzer will stop and will not try to execute the next steps.
//!
//! If you want to know more about the steps, you can check the [`steps`] module.
//!
//! The main usage of the analyzer is to verify that the sources are valid before
//! compiling them.
//! If you also need to keep the state of the analysis, you can use the [`Analyzer`]
//! struct directly, that offers a more fine-grained control over the analysis.

#![allow(dead_code)]

use crate::diagnostic::Diagnostic;
use crate::engine::Engine;
use crate::importer::{ASTImporter, Imported};
use crate::imports::Imports;
use crate::name::Name;
use crate::reef::Externals;
use crate::relations::{Relations, SourceId};
use crate::steps::collect::SymbolCollector;
use crate::steps::resolve_sources;
use crate::steps::typing::apply_types;
use crate::types::ctx::TypeContext;
use crate::types::engine::TypedEngine;
use crate::types::Typing;
use std::collections::HashSet;

pub mod diagnostic;
pub mod engine;
pub mod environment;
pub mod importer;
pub mod name;
pub mod relations;

mod dependency;
pub mod imports;
pub mod reef;
pub mod steps;
pub mod types;

/// Discovers the sources that are imported by the given source in the importer.
///
/// The returned analyzer contains the discovered sources and the diagnostics
/// that were generated, be sure to check them for errors.
pub fn analyze<'a>(
    entry_point: Name,
    importer: &mut impl ASTImporter,
    externals: &Externals,
) -> Analyzer<'a> {
    let mut analyzer = Analyzer::new();
    analyzer.process(entry_point, importer, externals);
    analyzer
}

/// Processes sources to resolve symbols and apply types.
#[derive(Default)]
pub struct Analyzer<'a> {
    /// The current state of the resolution.
    pub resolution: ResolutionResult<'a>,

    /// The current type knowledge.
    pub typing: Typing,

    pub type_context: TypeContext,

    /// The applied types over the [`Engine`].
    pub engine: TypedEngine,

    /// The diagnostics that were generated during the analysis.
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Analyzer<'a> {
    /// Creates a new empty analyzer.
    pub fn new() -> Self {
        Self::default()
    }

    /// Analyse starting from the given entry point.
    pub fn process(
        &mut self,
        entry_point: Name,
        importer: &mut impl ASTImporter,
        externals: &Externals,
    ) -> Analysis<'a, '_> {
        let last_next_source_id = SourceId(self.resolution.engine.len());
        resolve_sources(
            vec![entry_point],
            &mut self.resolution,
            importer,
            externals,
            &mut self.diagnostics,
        );
        if self.diagnostics.is_empty() {
            let (engine, context, typing) = apply_types(
                &self.resolution.engine,
                &self.resolution.relations,
                externals,
                &mut self.diagnostics,
            );
            self.engine = engine;
            self.type_context = context;
            self.typing = typing;
        }
        Analysis {
            analyzer: self,
            last_next_source_id,
        }
    }

    /// Performs an injection of a source as it would be a new entry point.
    ///
    /// # Panics
    /// If the injection refers to itself, this method will panic.
    pub fn inject(
        &mut self,
        inject: Inject,
        importer: &mut impl ASTImporter,
        externals: &Externals,
    ) -> Analysis<'a, '_> {
        let last_next_source_id = SourceId(self.resolution.engine.len());
        let name = inject.name.clone();
        let mut visit = vec![name.clone()];

        self.diagnostics.extend(SymbolCollector::inject(
            inject,
            &mut self.resolution.engine,
            &mut self.resolution.relations,
            &mut self.resolution.imports,
            externals,
            &mut visit,
        ));
        self.resolution.visited.insert(name);

        resolve_sources(
            visit,
            &mut self.resolution,
            importer,
            externals,
            &mut self.diagnostics,
        );
        if self.diagnostics.is_empty() {
            let (engine, context, typing) = apply_types(
                &self.resolution.engine,
                &self.resolution.relations,
                externals,
                &mut self.diagnostics,
            );
            self.engine = engine;
            self.type_context = context;
            self.typing = typing;
        }
        Analysis {
            analyzer: self,
            last_next_source_id,
        }
    }

    /// Takes the diagnostics that were generated during the analysis
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }
}

/// An analysis result that can be observed and reverted.
pub struct Analysis<'a, 'revert> {
    /// Takes the unique ownership of the analyzer to prevent any further modification
    /// that would invalidate any revert.
    analyzer: &'revert mut Analyzer<'a>,

    /// Reverting the operation means internally removing all the sources that were added
    /// after the last stable state.
    last_next_source_id: SourceId,
}

impl Analysis<'_, '_> {
    /// Gets a immutable reference to the analyzer, in order to preview the changes.
    ///
    /// To get back a mutable reference, simply drop the [`Analysis`] or call
    /// [`Analysis::revert`].
    pub fn analyzer(&self) -> &Analyzer<'_> {
        self.analyzer
    }

    /// Returns the source id of the injected source.
    pub fn attributed_id(&self) -> SourceId {
        SourceId(self.last_next_source_id.0)
    }

    #[must_use = "This method does not revert the analysis, `Analysis::revert` must be called"]
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.analyzer.diagnostics)
    }

    /// Performs a one-way operation to revert all the changes made by the injection.
    ///
    /// This drops the [`Analyzer`] unique ownership.
    pub fn revert(self) {
        let id = self.last_next_source_id;
        let resolution = &mut self.analyzer.resolution;
        for (_, _, env) in resolution.engine.origins.drain(id.0..) {
            if let Some(env) = env {
                resolution.visited.remove(&env.fqn);
            }
        }
        resolution.relations.retain_before(id);
        resolution.imports.retain_before(id);
    }
}

/// Performs a full resolution of the environments directly or indirectly implied by the entry point.
///
/// The completion of a collection followed by its resolution phase is called a cycle.
/// Multiple cycles can occur if the resolution phase finds new modules to collect.
pub fn resolve_all<'a>(
    entry_point: Name,
    externals: &'a Externals,
    importer: &mut impl ASTImporter,
    diagnostics: &mut Vec<Diagnostic>,
) -> ResolutionResult<'a> {
    let mut result = ResolutionResult::default();
    resolve_sources(
        vec![entry_point],
        &mut result,
        importer,
        externals,
        diagnostics,
    );
    result
}

/// A specially crafted input to inject at a specific point in the analyzer.
pub struct Inject {
    /// The name of the source to inject.
    pub name: Name,

    /// The imported content.
    pub imported: Imported,

    /// The environment to inject the source into.
    pub attached: Option<SourceId>,
}

/// The results of an analysis
#[derive(Debug, Default)]
pub struct ResolutionResult<'e> {
    pub engine: Engine<'e>,
    pub relations: Relations,
    imports: Imports,
    visited: HashSet<Name>,
}
