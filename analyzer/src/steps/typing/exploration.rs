use crate::engine::Engine;
use crate::environment::symbols::Symbol;
use crate::environment::Environment;
use crate::reef::{Externals, Reef, ReefId};
use crate::relations::{LocalId, Relations, ResolvedSymbol, SourceId, SymbolRef};
use crate::steps::typing::bounds::TypesBounds;
use crate::steps::typing::function::Return;
use crate::steps::typing::view::TypeView;
use crate::types::ctx::{TypeContext, TypedVariable};
use crate::types::engine::{Chunk, FunctionId, StructureId, TypedEngine};
use crate::types::ty::{FunctionDesc, MethodType, StructureDesc, Type, TypeRef};
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

impl<'a> Exploration<'a> {
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

    pub(super) fn get_chunk(&self, reef: ReefId, source: SourceId) -> Option<&Chunk> {
        let engine = if reef == self.externals.current {
            &self.type_engine
        } else {
            &self.get_external_type_reef(reef).typed_engine
        };
        engine.get_user(source)
    }

    pub(super) fn get_type_name(&self, ty: TypeRef) -> Option<&String> {
        if ty.reef == self.externals.current {
            self.typing.get_type_name(ty.type_id)
        } else {
            self.externals
                .get_reef(ty.reef)
                .unwrap()
                .typing
                .get_type_name(ty.type_id)
        }
    }

    /// Gets the type instance of a type identifier.
    pub(super) fn new_type_view(&self, id: TypeRef, bounds: &'a TypesBounds) -> TypeView {
        TypeView::new(id, self, bounds)
    }

    /// Gets the type of a generic type parameter.
    pub(super) fn concretize(&self, generic: TypeRef, instance_holder: TypeRef) -> TypeRef {
        if self.get_type(generic) != Some(&Type::Polytype) {
            return generic;
        }

        if let Some(&Type::Instantiated(base, ref parameters)) = self.get_type(instance_holder) {
            let Type::Structure(_, structure_id) = self.get_type(base).unwrap() else {
                panic!("instantiated type does not have a defined structure");
            };
            let base_type_reef = self.get_base_type(instance_holder).reef;
            let polytypes = &self
                .get_structure(base_type_reef, *structure_id)
                .unwrap()
                .type_parameters;

            let type_id = *polytypes
                .iter()
                .zip(parameters.iter())
                .find_map(|(generic_id, concrete)| {
                    (generic.reef == base_type_reef && generic.type_id == *generic_id)
                        .then_some(concrete)
                })
                .expect("Polytype should be instantiated.");
            return type_id;
        }
        generic
    }

    pub(super) fn get_method_exact(
        &self,
        id: TypeRef,
        name: &str,
        params: &[TypeRef],
        return_ty: TypeRef,
    ) -> Option<(&MethodType, FunctionId)> {
        let definition = self.get_base_type(id);
        let current = self.externals.current;

        let &Type::Structure(_, structure_id) = self.get_type(id).unwrap() else {
            return None;
        };

        if definition.reef == current || id.reef == current {
            self.type_engine
                .get_method_exact(structure_id, name, params, return_ty)
        } else {
            let reef = self.get_external_type_reef(id.reef);
            reef.typed_engine
                .get_method_exact(structure_id, name, params, return_ty)
        }
    }

    pub(super) fn get_symbol(
        &self,
        reef: ReefId,
        source: SourceId,
        local_id: LocalId,
        links: Links<'a>,
    ) -> Option<&'a Symbol> {
        let engine = if self.externals.current == reef {
            links.engine
        } else {
            &self.externals.get_reef(reef).unwrap().engine
        };

        engine
            .get_environment(source)
            .unwrap()
            .symbols
            .get(local_id)
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

    pub(super) fn get_methods(&self, id: TypeRef, name: &str) -> Option<&Vec<FunctionId>> {
        let definition = self.get_base_type(id);

        let &Type::Structure(_, structure_id) = self.get_type(definition).unwrap() else {
            return None;
        };

        if definition.reef == self.externals.current {
            self.type_engine.get_methods(structure_id, name)
        } else {
            let reef = self.get_external_type_reef(definition.reef);
            reef.typed_engine.get_methods(structure_id, name)
        }
    }

    /// Gets the base type of a type identifier.
    pub(crate) fn get_base_type(&self, type_id: TypeRef) -> TypeRef {
        match self.get_type(type_id).unwrap_or(&Type::Error) {
            Type::Instantiated(def, _) => *def,
            _ => type_id,
        }
    }

    pub(super) fn get_external_env(
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

    pub(super) fn get_function(
        &self,
        reef: ReefId,
        function_id: FunctionId,
    ) -> Option<&FunctionDesc> {
        if reef == self.externals.current {
            self.type_engine.get_function(function_id)
        } else {
            self.get_external_type_reef(reef)
                .typed_engine
                .get_function(function_id)
        }
    }

    pub(super) fn get_structure(
        &self,
        reef: ReefId,
        structure_id: StructureId,
    ) -> Option<&StructureDesc> {
        if reef == self.externals.current {
            self.type_engine.get_structure(structure_id)
        } else {
            self.get_external_type_reef(reef)
                .typed_engine
                .get_structure(structure_id)
        }
    }

    pub(super) fn is_compatible(&self, assign_to: TypeRef, rvalue: TypeRef) -> bool {
        if assign_to.is_err() || rvalue.is_err() || rvalue.is_nothing() {
            return true; // An error is compatible with everything, as it is a placeholder.
        }
        let lhs = self.get_type(assign_to).unwrap();
        if *lhs == Type::Polytype {
            return true;
        }

        let rhs = self.get_type(rvalue).unwrap();

        if let (Type::Instantiated(base_lhs, _), Type::Instantiated(base_rhs, _)) = (lhs, rhs) {
            return base_lhs == base_rhs;
        }

        lhs == rhs
    }

    fn get_external_type_reef(&self, id: ReefId) -> &Reef {
        self.externals
            .get_reef(id)
            .expect("Unknown external reef found on type")
    }
}
