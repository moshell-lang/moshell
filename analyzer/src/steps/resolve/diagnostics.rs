//! contains diagnostics only emitted by the resolution state

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::environment::Environment;
use crate::imports::SourceImports;
use crate::name::Name;
use crate::relations::{RelationId, SourceId, Symbol};
use context::source::SourceSegment;

/// Creates a diagnostic for a symbol being invalidated due to it's invalid import bound.
/// The caller must ensure that env_id is valid as well as the given name's root is contained in given env's variables.
pub fn diagnose_invalid_symbol_from_dead_import(
    engine: &Engine,
    env_id: SourceId,
    env_imports: &SourceImports,
    relation: RelationId,
    name: &Name,
) -> Diagnostic {
    let name_root = name.root();

    let env = engine.get_environment(env_id).expect("invalid env id");
    let segments = env.find_references(Symbol::External(relation));

    let msg = format!("unresolvable symbol `{name}` has no choice but to be ignored due to invalid import of `{name_root}`.");
    let invalid_import_seg = env_imports
        .get_import_segment(name_root)
        .expect("unknown import");

    let mut segments: Vec<_> = segments
        .iter()
        .map(|seg| Observation::new(seg.clone()).with_tag(1))
        .collect();

    segments.sort_by_key(|s| s.segment.start);

    Diagnostic::new(DiagnosticID::InvalidSymbol, env_id, msg)
        .with_observation(
            Observation::new(invalid_import_seg)
                .with_help("invalid import introduced here")
                .with_tag(0),
        )
        .with_observations(segments)
}

/// Appends a diagnostic for an external symbol that could not be resolved.
///
/// Each expression that use this symbol (such as variable references) will then get an observation.
pub fn diagnose_unresolved_external_symbols(
    relation: RelationId,
    env_id: SourceId,
    env: &Environment,
    name: &Name,
) -> Diagnostic {
    let diagnostic = Diagnostic::new(
        DiagnosticID::UnknownSymbol,
        env_id,
        format!("Could not resolve symbol `{name}`."),
    );

    let mut observations: Vec<_> = env
        .list_definitions()
        .filter(|(_, sym)| match sym {
            Symbol::Local(_) => false,
            Symbol::External(g) => *g == relation,
        })
        .map(|(seg, _)| Observation::new(seg.clone()).with_tag(0))
        .collect();

    observations.sort_by_key(|s| s.segment.start);
    diagnostic.with_observations(observations)
}

/// Appends a diagnostic for an import that could not be resolved.
/// Each `use` expressions that was referring to the unknown import will get a diagnostic
pub fn diagnose_unresolved_import(
    env_id: SourceId,
    imported_symbol_name: &Name,
    known_parent: Option<Name>,
    dependent_segment: SourceSegment,
) -> Diagnostic {
    let msg = format!(
        "unable to find imported symbol `{}`{}.",
        known_parent
            .as_ref()
            .and_then(|p| imported_symbol_name.relative_to(p))
            .unwrap_or(imported_symbol_name.clone()),
        known_parent
            .map(|p| format!(" in module `{p}`"))
            .unwrap_or_default()
    );

    Diagnostic::new(DiagnosticID::ImportResolution, env_id, msg)
        .with_observation(Observation::new(dependent_segment))
}
