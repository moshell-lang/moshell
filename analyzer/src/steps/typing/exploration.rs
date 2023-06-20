use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::relations::SourceId;
use crate::steps::typing::function::Return;
use crate::types::ctx::TypeContext;
use crate::types::engine::TypedEngine;
use crate::types::hir::TypeId;
use crate::types::ty::Type;
use crate::types::Typing;
use context::source::SourceSegment;

/// The support for type analysis.
pub(super) struct Exploration {
    pub(super) engine: TypedEngine,
    pub(super) typing: Typing,
    pub(super) ctx: TypeContext,
    pub(super) returns: Vec<Return>,
}

impl Exploration {
    pub(super) fn prepare(&mut self) {
        self.returns.clear();
    }

    pub(super) fn get_type(&self, id: TypeId) -> Option<&Type> {
        self.typing.get_type(id)
    }
}

/// Generates a diagnostic for an unknown type annotation.
pub(super) fn diagnose_unknown_type(source: SourceId, segment: SourceSegment) -> Diagnostic {
    Diagnostic::new(DiagnosticID::UnknownType, source, "Unknown type annotation")
        .with_observation(Observation::with_help(segment, "Not found in scope"))
}
