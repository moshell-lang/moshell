use crate::engine::Engine;
use crate::environment::Environment;
use crate::reef::{Externals, Reef, ReefId};
use crate::relations::{Definition, Relations, ResolvedSymbol, SourceId, SymbolRef};
use crate::steps::typing::function::Return;
use crate::types::ctx::{TypeContext, TypedVariable};
use crate::types::engine::{CodeEntry, TypedEngine};
use crate::types::ty::{MethodType, Type, TypeRef};
use crate::types::Typing;

/// The support for type analysis.
pub(super) struct Exploration<'a> {
    pub(super) type_engine: TypedEngine,
    pub(super) typing: Typing,
    pub(super) ctx: TypeContext,
    pub(super) returns: Vec<Return>,
    pub(super) externals: &'a Externals<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct Links<'a> {
    pub(super) source: SourceId,
    pub(super) engine: &'a Engine<'a>,
    pub(super) relations: &'a Relations,
}

impl<'a> Links<'a> {
    pub(super) fn env(self) -> &'a Environment {
        self.engine.get_environment(self.source).unwrap()
    }

    pub(super) fn with_source(self, source: SourceId) -> Self {
        Self { source, ..self }
    }
}

impl Exploration<'_> {
    pub(super) fn prepare(&mut self) {
        self.returns.clear();
    }

    pub(super) fn get_type(&self, id: TypeRef) -> Option<&Type> {
        let typing = if id.reef == self.externals.current {
            &self.typing
        } else {
            &self.get_external_type_reef(id.reef).typing
        };
        typing.get_type(id.type_id)
    }

    pub(super) fn get_method_exact(
        &self,
        id: TypeRef,
        name: &str,
        params: &[TypeRef],
        return_ty: TypeRef,
    ) -> Option<&MethodType> {
        let typed_engine = if id.reef == self.externals.current {
            &self.type_engine
        } else {
            &self.get_external_type_reef(id.reef).typed_engine
        };
        typed_engine.get_method_exact(id.type_id, name, params, return_ty)
    }

    pub(super) fn get_var(
        &self,
        source: SourceId, /* FIXME */
        symbol: SymbolRef,
        relations: &Relations,
    ) -> Option<TypedVariable> {
        let reef_id = match symbol {
            SymbolRef::Local(_) => return self.ctx.get(relations, source, symbol),
            SymbolRef::External(ext) => {
                let call_symbol = relations[ext]
                    .state
                    .expect_resolved("Unresolved symbol during typechecking");
                call_symbol.reef
            }
        };
        let ctx = if reef_id == self.externals.current {
            &self.ctx
        } else {
            &self
                .externals
                .get_reef(reef_id)
                .expect("Unknown external reef found on symbol")
                .type_context
        };
        ctx.get(relations, source, symbol)
    }

    pub(super) fn get_methods(&self, id: TypeRef, name: &str) -> Option<&Vec<MethodType>> {
        let typed_engine = if id.reef == self.externals.current {
            &self.type_engine
        } else {
            &self.get_external_type_reef(id.reef).typed_engine
        };
        typed_engine.get_methods(id.type_id, name)
    }

    pub(super) fn get_external_env<'a>(
        &'a self,
        from_env: &'a Environment,
        to_symbol: ResolvedSymbol,
    ) -> Option<&'a Environment> {
        if to_symbol.reef == self.externals.current {
            Some(from_env)
        } else {
            self.externals
                .get_reef(to_symbol.reef)
                .unwrap()
                .engine
                .get_environment(to_symbol.source)
        }
    }

    pub(super) fn get_types(&self, reef: ReefId) -> Option<&Typing> {
        if reef == self.externals.current {
            Some(&self.typing)
        } else {
            self.externals.get_reef(reef).map(|r| &r.typing)
        }
    }

    pub(super) fn get_entry(&self, reef: ReefId, definition: Definition) -> Option<CodeEntry> {
        if reef == self.externals.current {
            self.type_engine.get(definition)
        } else {
            self.get_external_type_reef(reef)
                .typed_engine
                .get(definition)
        }
    }

    fn get_external_type_reef(&self, id: ReefId) -> &Reef {
        self.externals
            .get_reef(id)
            .expect("Unknown external reef found on type")
    }
}
