use crate::relations::SourceObjectId;
use crate::steps::typing::function::Return;
use crate::types::ctx::TypeContext;
use crate::types::engine::TypedEngine;
use crate::types::hir::TypeId;
use crate::types::ty::Type;
use crate::types::Typing;

/// The support for type analysis.
pub(crate) struct Exploration {
    pub(crate) engine: TypedEngine,
    pub(crate) typing: Typing,
    pub(crate) ctx: TypeContext,
    pub(crate) returns: Vec<Return>,
}

impl Exploration {
    pub(crate) fn prepare(&mut self, source_id: SourceObjectId) {
        self.ctx.prepare(source_id);
        self.returns.clear();
    }

    pub(crate) fn get_type(&self, id: TypeId) -> Option<&Type> {
        self.typing.get_type(id)
    }
}
