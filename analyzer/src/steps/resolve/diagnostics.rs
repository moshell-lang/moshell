//! contains diagnostics only emitted by the resolution state

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::environment::Environment;
use crate::imports::SourceImports;
use crate::name::Name;
use crate::relations::{GlobalObjectId, SourceObjectId, Symbol};
use context::source::SourceSegment;

/// Creates a diagnostic for a symbol being invalidated due to it's invalid import bound.
/// The caller must ensure that env_id is valid as well as the given name's root is contained in given env's variables.
pub fn diagnose_invalid_symbol_from_dead_import(
    engine: &Engine,
    env_id: SourceObjectId,
    env_imports: &SourceImports,
    global_relation: GlobalObjectId,
    name: &Name,
) -> Diagnostic {
    let name_root = name.root();

    let env = engine.get_environment(env_id).expect("invalid env id");
    let segments = env.find_references(global_relation.into());

    let msg = format!("unresolvable symbol `{name}` has no choice but to be ignored due to invalid import of `{name_root}`.");
    let invalid_import_seg = env_imports
        .get_import_segment(name_root)
        .expect("unknown import");

    let mut segments: Vec<_> = segments
        .iter()
        .map(|seg| Observation::new(seg.clone()))
        .collect();

    segments.sort_by_key(|s| s.segment.start);

    Diagnostic::new(DiagnosticID::InvalidSymbol, env_id, msg)
        .with_observation(Observation::with_help(
            invalid_import_seg,
            "invalid import introduced here",
        ))
        .with_observations(segments)
}

/// Appends a diagnostic for an external symbol that could not be resolved.
///
/// Each expression that use this symbol (such as variable references) will then get an observation.
pub fn diagnose_unresolved_external_symbols(
    relation: GlobalObjectId,
    env_id: SourceObjectId,
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
            Symbol::Global(g) => *g == relation.0,
        })
        .map(|(seg, _)| Observation::new(seg.clone()))
        .collect();

    observations.sort_by_key(|s| s.segment.start);
    diagnostic.with_observations(observations)
}

/// Appends a diagnostic for an import that could not be resolved.
/// Each `use` expressions that was referring to the unknown import will get a diagnostic
pub fn diagnose_unresolved_import(
    env_id: SourceObjectId,
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
