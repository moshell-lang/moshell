use std::fmt::Debug;
use std::ops::{Index, IndexMut};

use context::source::SourceSegment;

use crate::dependency::Dependencies;
use crate::engine::Engine;
use crate::environment::symbols::{SymbolInfo, SymbolRegistry};
use crate::name::Name;
use crate::reef::{ReefId, LANG_REEF};

/// The object identifier base.
///
/// An object is anything that can be referenced by its [`ObjectId`],
/// here's an exhaustive list of the main objects and structures in the analyzer.
///
/// - [`RelationId`] points to a relation in [`Relation`].
/// - [`SourceId`] points to a source in [`Engine`], sources are a root AST expression bound to an [`Environment`].
/// - [`LocalId`] points to a local object in an environment's [`crate::environment::symbols::Symbols`].
/// - [`NativeId`] refers to an intrinsic function or method.
/// - [`crate::types::TypeId`] points to a type registered in [`Typing`]
///
/// Some main structures are based on object identifiers
/// - [`ResolvedSymbol`] contains a [`SourceId`] and a [`LocalId`] which globally targets a symbol inside a given source environment,
///                      this structure is emitted by the resolution phase.
/// - [`SymbolRef`] refers to a symbol, which is either local (to an unbound environment) or external, where the relation is held by the [`Relations`]
pub type ObjectId = usize;

/// A relation identifier, that points to a specific relation in the [`Relations`].
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct RelationId(pub ObjectId);

/// A source identifier, that can be the target of a global resolution.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SourceId(pub ObjectId);

/// An identifier for a local variable stored in an environment
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LocalId(pub ObjectId);

/// An indication where an object is located.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum SymbolRef {
    /// A local object, referenced by its index in the [`Environment`] it is defined in.
    Local(LocalId),

    /// An external symbol, where the relation is contained in the [`Resolver`].
    External(RelationId),
}

impl From<RelationId> for SymbolRef {
    fn from(id: RelationId) -> Self {
        SymbolRef::External(id)
    }
}

impl From<LocalId> for SymbolRef {
    fn from(id: LocalId) -> Self {
        SymbolRef::Local(id)
    }
}

/// The resolved information about a symbol.
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct ResolvedSymbol {
    /// The symbol's reef
    pub reef: ReefId,
    /// The module where the symbol is defined.
    ///
    /// This is used to route the symbol to the correct environment.
    pub source: SourceId,

    /// The object identifier of the symbol, local to the given environment.
    pub object_id: LocalId,
}

impl ResolvedSymbol {
    pub const fn new(reef: ReefId, source: SourceId, object_id: LocalId) -> Self {
        Self {
            reef,
            source,
            object_id,
        }
    }

    pub const fn lang_symbol(object_id: LocalId) -> Self {
        Self {
            reef: LANG_REEF,
            source: SourceId(0),
            object_id,
        }
    }
}

/// The state of a relation
///
/// The [SymbolResolver] only attempts to resolve relations marked as [RelationState::Unresolved]
/// If the resolution fails, for any reason, the object is marked as dead ([RelationState::Dead])
/// which in most case implies a diagnostic.
/// The dead state prevents the resolver to attempt to resolve again unresolvable symbols on next cycles.
/// If the relation was successfully resolved, the state is then [RelationState::Resolved], containing the
/// resolved symbol and targeted environment.
#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub enum RelationState {
    Resolved(ResolvedSymbol),
    Unresolved,
    Dead,
}

impl RelationState {
    pub fn expect_resolved(self, msg: &str) -> ResolvedSymbol {
        match self {
            RelationState::Resolved(resolved) => resolved,
            _ => panic!("{}", msg),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq)]
pub struct Relation {
    /// The environment's id that requested this object resolution.
    pub origin: SourceId,

    /// This relation's state.
    /// See [RelationState] for more details
    pub state: RelationState,

    /// The targeted registry of the symbol
    pub registry: SymbolRegistry,
}

impl Relation {
    pub fn unresolved(origin: SourceId, registry: SymbolRegistry) -> Self {
        Self {
            origin,
            state: RelationState::Unresolved,
            registry,
        }
    }

    pub fn resolved(origin: SourceId, resolved: ResolvedSymbol, registry: SymbolRegistry) -> Self {
        Self {
            origin,
            state: RelationState::Resolved(resolved),
            registry,
        }
    }
}

/// A collection of objects that are tracked globally and may link to each other.
#[derive(Debug, Default)]
pub struct Relations {
    /// The tracked relations between environments.
    ///
    /// The actual [`String`] -> [`ObjectId`] mapping is left to the [`crate::environment::Environment`].
    /// The reason that the resolution information is lifted out of the environment is that identifiers
    /// binding happens across modules, and an environment cannot guarantee that it will be able to generate
    /// unique identifiers for all the symbols that do not conflicts with the ones from other modules.
    relations: Vec<Relation>,
}

impl Relations {
    /// Tracks a new object and returns its identifier.
    pub fn track_new_object(&mut self, origin: SourceId, registry: SymbolRegistry) -> RelationId {
        let id = self.relations.len();
        self.relations.push(Relation::unresolved(origin, registry));
        RelationId(id)
    }

    /// Finds segments that reference the given object.
    ///
    /// Returns [`None`] if the object is neither found nor tracked.
    pub fn find_references(
        &self,
        engine: &Engine,
        tracked_object: RelationId,
    ) -> Option<Vec<SourceSegment>> {
        let object = self.relations.get(tracked_object.0)?;
        let environment = engine
            .get_environment(object.origin)
            .expect("object relation targets to an unknown environment");
        Some(environment.find_references(SymbolRef::External(tracked_object)))
    }

    /// Returns a mutable iterator over all the objects.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (RelationId, &mut Relation)> {
        self.relations
            .iter_mut()
            .enumerate()
            .map(|(id, relation)| (RelationId(id), relation))
    }

    /// Returns an immutable iterator over all the objects.
    pub fn iter(&self) -> impl Iterator<Item = (RelationId, &Relation)> {
        self.relations
            .iter()
            .enumerate()
            .map(|(id, relation)| (RelationId(id), relation))
    }

    /// Returns the state of the given object.
    ///
    /// If the relation is not referenced, returns [`None`].
    pub fn get_state(&self, id: RelationId) -> Option<RelationState> {
        Some(self.relations.get(id.0)?.state)
    }

    /// Removes all the objects that have been created at or after the given id.
    pub fn retain_before(&mut self, id: SourceId) {
        self.relations.retain(|relation| relation.origin.0 < id.0);
    }

    /// Creates a dependency graph for the given engine.
    /// only symbols that are inside of the engine's reef are placed in the graph.
    pub fn as_dependencies(&self, engine_reef: ReefId, engine: &Engine) -> Dependencies<SourceId> {
        let mut dependencies = Dependencies::default();
        for (env_id, env) in engine.environments() {
            dependencies.add_node(env_id);

            for (seg, symbol) in env.definitions.iter() {
                //TODO avoid annotating declarations to remove this check
                if env.declarations.contains_key(seg) {
                    continue; //a declaration is not a dependency
                }

                match symbol {
                    SymbolRef::Local(lid) => {
                        let symbol = env.symbols.get(*lid).unwrap();
                        if symbol.ty == SymbolInfo::Type {
                            let symbol_fqn =
                                env.fqn.appended(Name::from(vec![symbol.name.to_string()]));
                            if let Some((dependency_env_id, _)) =
                                engine.find_environment_by_name(&symbol_fqn)
                            {
                                dependencies.add_dependency(env_id, dependency_env_id);
                            }
                        }
                    }
                    SymbolRef::External(e) => {
                        let object = &self[*e];
                        if let RelationState::Resolved(resolved) = object.state {
                            if resolved.reef == engine_reef {
                                dependencies.add_dependency(env_id, resolved.source);
                            }
                        }
                    }
                }
            }
        }
        dependencies
    }
}

impl IndexMut<RelationId> for Relations {
    fn index_mut(&mut self, index: RelationId) -> &mut Self::Output {
        &mut self.relations[index.0]
    }
}

impl Index<RelationId> for Relations {
    type Output = Relation;

    fn index(&self, index: RelationId) -> &Self::Output {
        &self.relations[index.0]
    }
}
