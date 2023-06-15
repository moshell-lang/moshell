use crate::steps::typing::function::Return;
use crate::types::ctx::TypeContext;
use crate::types::engine::TypedEngine;
use crate::types::hir::TypeId;
use crate::types::ty::Type;
use crate::types::Typing;

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
