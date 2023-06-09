use std::fmt::Debug;

use context::source::SourceSegment;

use crate::engine::Engine;

/// The object identifier base.
///
/// Note that this type doesn't convey if it is a local or global object, i.e. in which scope it is stored.
///
/// To further indicate the provenance of the object, use specific types:
/// - [`GlobalObjectId`] points to a global object that needs to be resolved.
/// - [`SourceObjectId`] points to a object whose environment may contain actual symbol sources.
/// - [`ResolvedSymbol`] is used to point globally to a nested environment.
/// - [`Symbol`] refers differentiate a id that is local or not.
pub type ObjectId = usize;

/// A global object identifier, that points to a specific object in the [`Relations`].
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct GlobalObjectId(pub ObjectId);

/// A source object identifier, that can be the target of a global resolution.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SourceObjectId(pub ObjectId);

/// An indication where an object is located.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    /// A local object, referenced by its index in the [`crate::environment::Environment`] it is defined in.
    Local(ObjectId),

    /// A global object, referenced by its index in the [`Relations`] it is linked to.
    Global(ObjectId),
}

impl From<GlobalObjectId> for Symbol {
    fn from(id: GlobalObjectId) -> Self {
        Symbol::Global(id.0)
    }
}

/// The resolved information about a symbol.
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct ResolvedSymbol {
    /// The module where the symbol is defined.
    ///
    /// This is used to route the symbol to the correct environment.
    pub source: SourceObjectId,

    /// The object identifier of the symbol, local to the module.
    pub object_id: ObjectId,
}

impl ResolvedSymbol {
    pub fn new(source: SourceObjectId, object_id: ObjectId) -> Self {
        Self { source, object_id }
    }
}

/// The state of an object
///
/// The [SymbolResolver] only handles objects marked as [ObjectState::Unresolved]
/// and will attempt to resolve it by updating his state to [ObjectState::Resolved]
/// If the resolution fails, for any reason, the object is marked as dead ([ObjectState::Dead])
/// which should imply a diagnostic.
/// This state prevents the resolver to attempt to resolve again unresolvable symbols on next cycles.
#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub enum ObjectState {
    Resolved(ResolvedSymbol),
    Unresolved,
    Dead,
}

#[derive(Debug, Clone, Hash, PartialEq)]
pub struct Object {
    /// The environment's id that requested this object resolution.
    pub origin: SourceObjectId,

    /// This object's state.
    /// See [ObjectState] for more details
    pub state: ObjectState,
}

impl Object {
    pub fn unresolved(origin: SourceObjectId) -> Self {
        Self {
            origin,
            state: ObjectState::Unresolved,
        }
    }

    pub fn resolved(origin: SourceObjectId, resolved: ResolvedSymbol) -> Self {
        Self {
            origin,
            state: ObjectState::Resolved(resolved),
        }
    }
}

/// A collection of objects that are tracked globally and may link to each other.
#[derive(Debug, Default)]
pub struct Relations {
    /// The objects that need resolution that are tracked globally.
    ///
    /// The actual [`String`] -> [`ObjectId`] mapping is left to the [`crate::environment::Environment`].
    /// The reason that the resolution information is lifted out of the environment is that identifiers
    /// binding happens across modules, and an environment cannot guarantee that it will be able to generate
    /// unique identifiers for all the symbols that do not conflicts with the ones from other modules.
    pub objects: Vec<Object>,
}

impl Relations {
    /// Tracks a new object and returns its identifier.
    pub fn track_new_object(&mut self, origin: SourceObjectId) -> GlobalObjectId {
        let id = self.objects.len();
        self.objects.push(Object::unresolved(origin));
        GlobalObjectId(id)
    }

    /// Finds segments that reference the given object.
    ///
    /// Returns [`None`] if the object is neither found nor tracked.
    pub fn find_references(
        &self,
        engine: &Engine,
        tracked_object: GlobalObjectId,
    ) -> Option<Vec<SourceSegment>> {
        let object = self.objects.get(tracked_object.0)?;
        let environment = engine
            .get_environment(object.origin)
            .expect("object relation targets to an unknown environment");
        Some(environment.find_references(Symbol::Global(tracked_object.0)))
    }

    /// Returns a mutable iterator over all the objects.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (GlobalObjectId, &mut Object)> {
        self.objects
            .iter_mut()
            .enumerate()
            .map(|(id, object)| (GlobalObjectId(id), object))
    }

    /// Returns the state of the given object.
    ///
    /// If the object is not referenced, returns [`None`].
    pub fn get_state(&self, id: GlobalObjectId) -> Option<ObjectState> {
        Some(self.objects.get(id.0)?.state)
    }
}
