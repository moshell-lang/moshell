use crate::name::Name;

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

/// A global object identifier, that points to a specific object in the [`Resolver`].
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

    /// A global object, referenced by its index in the [`Resolver`] it is linked to.
    Global(ObjectId),
}

/// The resolved information about a symbol.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedSymbol {
    /// The module where the symbol is defined.
    ///
    /// This is used to route the symbol to the correct environment.
    pub module: SourceObjectId,

    /// The object identifier of the symbol, local to the module.
    pub object_id: ObjectId,
}

impl From<GlobalObjectId> for Symbol {
    fn from(id: GlobalObjectId) -> Self {
        Symbol::Global(id.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Object {
    /// The symbol that is being resolved, where it is used.
    origin: SourceObjectId,

    /// The link to the resolved symbol.
    resolved: Option<ResolvedSymbol>,
}

/// A collection of objects that are tracked globally and may link to each other.
#[derive(Debug, Clone, Default)]
pub struct Resolver {
    /// The objects that need resolution that are tracked globally.
    ///
    /// The actual [`String`] -> [`ObjectId`] mapping is left to the [`crate::environment::Environment`].
    /// The reason that the resolution information is lifted out of the environment is that identifiers
    /// binding happens across modules, and an environment cannot guarantee that it will be able to generate
    /// unique identifiers for all the symbols that do not conflicts with the ones from other modules.
    pub objects: Vec<Object>,

    /// The list of modules that are yet to be visited.
    pub visitable: Vec<Name>,
}

impl Resolver {
    /// Tracks a new object and returns its identifier.
    pub fn track_new_object(&mut self, origin: SourceObjectId) -> GlobalObjectId {
        let id = self.objects.len();
        self.objects.push(Object {
            origin,
            resolved: None,
        });
        GlobalObjectId(id)
    }
}
