use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::relations::SourceObjectId;
use crate::steps::typing::function::Return;
use crate::steps::typing::lower::call_convert_on;
use crate::steps::typing::TypingState;
use crate::types::ctx::TypeContext;
use crate::types::engine::TypedEngine;
use crate::types::hir::{TypeId, TypedExpr};
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

    pub(super) fn unify(
        &mut self,
        assign_to: TypeId,
        expr: TypedExpr,
        diagnostics: &mut Vec<Diagnostic>,
        state: TypingState,
    ) -> Result<TypedExpr, TypedExpr> {
        match self.typing.unify(assign_to, expr.ty) {
            Ok(ty) => Ok(call_convert_on(
                expr,
                self,
                ty,
                |ty| format!("Cannot convert type `{}`", ty),
                diagnostics,
                state,
            )),
            Err(_) => Err(expr),
        }
    }
}

pub(super) fn diagnose_unknown_type(source: SourceObjectId, segment: SourceSegment) -> Diagnostic {
    Diagnostic::new(DiagnosticID::UnknownType, source, "Unknown type annotation")
        .with_observation(Observation::with_help(segment, "Not found in scope"))
}
