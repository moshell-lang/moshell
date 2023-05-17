use crate::engine::Engine;
use crate::name::Name;
use context::source::SourceSegment;
use std::collections::HashMap;

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

impl From<GlobalObjectId> for Symbol {
    fn from(id: GlobalObjectId) -> Self {
        Symbol::Global(id.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct UnresolvedImports {
    pub imports: Vec<UnresolvedImport>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnresolvedImport {
    Symbol { alias: Option<String>, name: Name },
    AllIn(Name),
}

impl UnresolvedImports {
    pub fn new(imports: Vec<UnresolvedImport>) -> Self {
        Self { imports }
    }

    pub fn add_unresolved_import(&mut self, import: UnresolvedImport) {
        self.imports.push(import)
    }
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

impl ResolvedSymbol {
    pub fn new(module: SourceObjectId, object_id: ObjectId) -> Self {
        Self { module, object_id }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Object {
    /// The symbol that is being resolved, where it is used.
    pub origin: SourceObjectId,

    /// The link to the resolved symbol.
    pub resolved: Option<ResolvedSymbol>,
}

impl Object {
    pub fn unresolved(origin: SourceObjectId) -> Self {
        Self {
            origin,
            resolved: None,
        }
    }

    pub fn resolved(origin: SourceObjectId, resolved: ResolvedSymbol) -> Self {
        Self {
            origin,
            resolved: Some(resolved),
        }
    }
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

    /// Associates a source object with its unresolved imports.
    ///
    /// Imports may only be declared at the top level of a source. This lets us track the unresolved imports
    /// per [`crate::environment::Environment`]. If a source is not tracked here, it means that it has no
    /// imports. This is only used to create find the link between environments and sources, and should not
    /// be used after the resolution is done.
    pub imports: HashMap<SourceObjectId, UnresolvedImports>,
}

impl Resolver {
    /// Take the imports
    pub fn take_imports(&mut self) -> HashMap<SourceObjectId, UnresolvedImports> {
        std::mem::take(&mut self.imports)
    }

    /// References a new import directive in the given source.
    ///
    /// This directive may be used later to resolve the import.
    pub fn add_import(&mut self, source: SourceObjectId, import: UnresolvedImport) {
        let imports = self
            .imports
            .entry(source)
            .or_insert_with(UnresolvedImports::default);
        imports.add_unresolved_import(import)
    }

    /// Tracks a new object and returns its identifier.
    pub fn track_new_object(&mut self, origin: SourceObjectId) -> GlobalObjectId {
        let id = self.objects.len();
        self.objects.push(Object {
            origin,
            resolved: None,
        });
        GlobalObjectId(id)
    }

    /// Finds segments that reference the given object.
    pub fn find_references(
        &self,
        engine: &Engine,
        tracked_object: GlobalObjectId,
    ) -> Option<Vec<SourceSegment>> {
        let object = self.objects.get(tracked_object.0)?;
        let environment = engine.get_environment(object.origin)?;
        Some(environment.find_references(Symbol::Global(tracked_object.0)))
    }

    /// Returns a mutable iterator over all the objects.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (GlobalObjectId, &mut Object)> {
        self.objects
            .iter_mut()
            .enumerate()
            .map(|(id, object)| (GlobalObjectId(id), object))
    }

    /// Returns the resolved symbol for the given object.
    ///
    /// If the object is not resolved or is not referenced, returns `None`.
    pub fn get_resolved(&self, id: GlobalObjectId) -> Option<ResolvedSymbol> {
        self.objects.get(id.0)?.resolved
    }
}
