use crate::engine::Engine;
use crate::environment::Environment;
use crate::reef::{Externals, Reef, ReefId};
use crate::relations::{Definition, Relations, ResolvedSymbol, SourceId, SymbolRef};
use crate::steps::typing::function::Return;
use crate::steps::typing::view::TypeInstance;
use crate::types::ctx::{TypeContext, TypedVariable};
use crate::types::engine::{CodeEntry, TypedEngine};
use crate::types::ty::{MethodType, Type, TypeDescription, TypeRef};
use crate::types::{DefinitionId, Typing};

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

    pub(super) fn get_type_ref(&self, id: TypeRef) -> Option<&Type> {
        let typing = if id.reef == self.externals.current {
            &self.typing
        } else {
            &self.get_external_type_reef(id.reef).typing
        };
        typing.get_type(id.type_id)
    }

    /// Gets the type instance of a type identifier.
    pub(super) fn get_type(&self, id: TypeRef) -> TypeInstance {
        TypeInstance::new(id, self)
    }

    /// Gets the type of a generic type parameter.
    pub(super) fn concretize(&self, generic: TypeRef, instance_holder: TypeRef) -> TypeInstance {
        if self.get_type_ref(generic) == Some(&Type::Polytype) {
            if let Some(&Type::Instantiated(polytype, ref parameters)) =
                self.get_type_ref(instance_holder)
            {
                let generics = &self.get_description(polytype).unwrap().generics;
                return TypeInstance::new(
                    *generics
                        .iter()
                        .zip(parameters.iter())
                        .find_map(|(generic_id, concrete)| {
                            (generic == *generic_id).then_some(concrete)
                        })
                        .expect("Polytype should be instantiated."),
                    self,
                );
            }
        }
        TypeInstance::new(generic, self)
    }

    pub(super) fn get_method_exact(
        &self,
        id: TypeRef,
        name: &str,
        params: &[TypeRef],
        return_ty: TypeRef,
    ) -> Option<&MethodType> {
        let definition = self.get_base_type(id);
        if definition.0.reef == self.externals.current {
            self.type_engine
                .get_method_exact(definition.0.type_id, name, params, return_ty)
        } else {
            let reef = self.get_external_type_reef(id.reef);
            reef.typed_engine
                .get_method_exact(definition.0.type_id, name, params, return_ty)
        }
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
        let definition = self.get_base_type(id);
        if definition.0.reef == self.externals.current {
            self.type_engine.get_methods(definition.0.type_id, name)
        } else {
            let reef = self.get_external_type_reef(id.reef);
            reef.typed_engine.get_methods(definition.0.type_id, name)
        }
    }

    pub(super) fn get_description(&self, id: TypeRef) -> Option<&TypeDescription> {
        let definition = self.get_base_type(id);
        if definition.0.reef == self.externals.current {
            self.type_engine.get_description(definition.0.type_id)
        } else {
            let reef = self.get_external_type_reef(id.reef);
            reef.typed_engine.get_description(definition.0.type_id)
        }
    }

    /// Gets the base type of a type identifier.
    pub(crate) fn get_base_type(&self, type_id: TypeRef) -> DefinitionId {
        DefinitionId(match self.get_type_ref(type_id).unwrap_or(&Type::Error) {
            Type::Instantiated(def, _) => *def,
            _ => type_id,
        })
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

    pub(super) fn is_compatible(&self, assign_to: TypeRef, rvalue: TypeRef) -> bool {
        if assign_to.is_err() || rvalue.is_err() || rvalue.is_nothing() {
            return true; // An error is compatible with everything, as it is a placeholder.
        }
        let lhs = self.get_type_ref(assign_to).unwrap();
        let rhs = self.get_type_ref(rvalue).unwrap();
        lhs == rhs
    }

    fn get_external_type_reef(&self, id: ReefId) -> &Reef {
        self.externals
            .get_reef(id)
            .expect("Unknown external reef found on type")
    }
}
