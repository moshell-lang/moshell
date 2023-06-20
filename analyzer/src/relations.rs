use crate::dependency::Dependencies;
use std::fmt::Debug;
use std::ops::{Index, IndexMut};

use context::source::SourceSegment;

use crate::engine::Engine;

/// The object identifier base.
///
/// An object is anything that can be referenced by its [`ObjectId`],
/// here's an exhaustive list of the main objects and structures in the analyzer.
///
/// - [`RelationId`] points to a relation in [`Relation`].
/// - [`SourceId`] points to a source in [`Engine`], sources are a root AST expression bound to an [`Environment`].
/// - [`LocalId`] points to a local object in an environment's [`crate::environment::variables::Variables`].
/// - [`NativeId`] refers to an intrinsic function or method.
/// - [`crate::types::TypeId`] points to a type registered in [`Typing`]
///
/// Some main structures are based on object identifiers
/// - [`ResolvedSymbol`] contains a [`SourceId`] and a [`LocalId`] which globally targets a symbol inside a given source environment,
///                      this structure is emitted by the resolution phase.
/// - [`Symbol`] refers to a symbol, which is either local (to an unbound environment) or external, where the relation is held by the [`Relations`]
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

/// A native object identifier, that points to a native function in the [`Engine`].
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NativeId(pub ObjectId);

/// An indication where an object is located.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    /// A local object, referenced by its index in the [`Environment`] it is defined in.
    Local(LocalId),

    /// An external symbol, where the relation is contained in the [`Resolver`].
    External(RelationId),
}

/// An identifier in an engine.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Definition {
    /// A block of code, defined by the user.
    User(SourceId),

    /// A native function, that is defined in the VM.
    ///
    /// It can also have a special treatment in the compiler, but it is abstracted away
    /// during the analysis.
    Native(NativeId),
}

impl Definition {
    /// Builds an erroneous definition that is used for error propagation.
    pub fn error() -> Self {
        Self::User(SourceId(ObjectId::MAX))
    }
}

impl From<RelationId> for Symbol {
    fn from(id: RelationId) -> Self {
        Symbol::External(id)
    }
}

impl From<LocalId> for Symbol {
    fn from(id: LocalId) -> Self {
        Symbol::Local(id)
    }
}

/// The resolved information about a symbol.
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct ResolvedSymbol {
    /// The module where the symbol is defined.
    ///
    /// This is used to route the symbol to the correct environment.
    pub source: SourceId,

    /// The object identifier of the symbol, local to the given environment.
    pub object_id: LocalId,
}

impl ResolvedSymbol {
    pub fn new(source: SourceId, object_id: LocalId) -> Self {
        Self { source, object_id }
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
}

impl Relation {
    pub fn unresolved(origin: SourceId) -> Self {
        Self {
            origin,
            state: RelationState::Unresolved,
        }
    }

    pub fn resolved(origin: SourceId, resolved: ResolvedSymbol) -> Self {
        Self {
            origin,
            state: RelationState::Resolved(resolved),
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
    pub fn track_new_object(&mut self, origin: SourceId) -> RelationId {
        let id = self.relations.len();
        self.relations.push(Relation::unresolved(origin));
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
        Some(environment.find_references(Symbol::External(tracked_object)))
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

    /// Creates a dependency graph for the given engine.
    pub fn as_dependencies(&self, engine: &Engine) -> Dependencies<SourceId> {
        let mut dependencies = Dependencies::default();
        for (id, _) in engine.environments() {
            dependencies.add_node(id);
        }

        for object in self.relations.iter() {
            if let RelationState::Resolved(resolved) = object.state {
                dependencies.add_dependency(object.origin, resolved.source);
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
