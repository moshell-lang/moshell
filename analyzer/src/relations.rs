use crate::engine::Engine;
use crate::name::Name;
use context::source::SourceSegment;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

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

/// The structure that hosts the unresolved and resolved imported symbols of an environment
#[derive(PartialEq, Default)]
pub struct Imports {
    /// The imports that still needs to be resolved.
    /// Binds an [UnresolvedImport] to the segment that introduced the import.
    unresolved_imports: IndexMap<UnresolvedImport, SourceSegment>,

    /// Binds a symbol name to its resolved import, with the source segment where the import is declared.
    imported_symbols: HashMap<String, (ResolvedImport, SourceSegment)>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum UnresolvedImport {
    /// A symbol import with an optional alias.
    Symbol {
        alias: Option<String>,
        qualified_name: Name,
    },
    /// Variant to target all the exported symbols of a symbol
    AllIn(Name),
}

/// A resolved symbol import
#[derive(PartialEq, Eq, Debug)]
pub enum ResolvedImport {
    /// The import is a symbol
    Symbol(ResolvedSymbol),
    /// The import is an environment
    Env(SourceObjectId),

    /// The import wasn't found
    Dead,
}

impl Debug for Imports {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut imported_symbols: Vec<_> = self.imported_symbols.iter().collect();
        imported_symbols.sort_by_key(|(k, _)| *k);
        f.debug_struct("Imports")
            .field("imported_symbols", &imported_symbols)
            .field("unresolved_imports", &self.unresolved_imports)
            .finish()
    }
}

impl Imports {
    pub fn new(unresolved_imports: IndexMap<UnresolvedImport, SourceSegment>) -> Self {
        Self {
            unresolved_imports,
            imported_symbols: HashMap::new(),
        }
    }

    #[cfg(test)]
    pub fn with(
        unresolved_imports: IndexMap<UnresolvedImport, SourceSegment>,
        imported_symbols: HashMap<String, (ResolvedImport, SourceSegment)>,
    ) -> Self {
        Self {
            unresolved_imports,
            imported_symbols,
        }
    }

    ///Adds an unresolved import, placing the given `import_expr` as the dependent .
    pub fn add_unresolved_import(
        &mut self,
        import: UnresolvedImport,
        segment: SourceSegment,
    ) -> Option<SourceSegment> {
        self.unresolved_imports.insert(import, segment)
    }

    pub fn take_unresolved_imports(&mut self) -> IndexMap<UnresolvedImport, SourceSegment> {
        std::mem::take(&mut self.unresolved_imports)
    }

    pub fn set_resolved_import(
        &mut self,
        name: String,
        resolved: ResolvedImport,
        segment: SourceSegment,
    ) {
        self.imported_symbols.insert(name, (resolved, segment));
    }

    pub fn get_import(&self, name: &str) -> Option<&ResolvedImport> {
        self.imported_symbols.get(name).map(|(i, _)| i)
    }

    pub fn get_import_segment(&self, name: &str) -> Option<SourceSegment> {
        self.imported_symbols.get(name).map(|(_, s)| s.clone())
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

    /// Associates a source object with its imports.
    ///
    /// Imports may only be declared at the top level of a source. This lets us track the unresolved imports
    /// per [`crate::environment::Environment`]. If a source is not tracked here, it means that it has no
    /// imports.
    imports: HashMap<SourceObjectId, Imports>,
}

impl Relations {
    /// References a new import directive in the given source.
    ///
    /// This directive may be used later to resolve the import.
    pub fn add_unresolved_import(
        &mut self,
        source: SourceObjectId,
        import: UnresolvedImport,
        import_expr: SourceSegment,
    ) -> Option<SourceSegment> {
        let imports = self.imports.entry(source).or_insert_with(Imports::default);
        imports.add_unresolved_import(import, import_expr)
    }

    pub fn get_imports(&self, source: SourceObjectId) -> Option<&Imports> {
        self.imports.get(&source)
    }

    pub fn get_imports_mut(&mut self, source: SourceObjectId) -> Option<&mut Imports> {
        self.imports.get_mut(&source)
    }

    /// Tracks a new object and returns its identifier.
    pub fn track_new_object(&mut self, origin: SourceObjectId) -> GlobalObjectId {
        let id = self.objects.len();
        self.objects.push(Object::unresolved(origin));
        GlobalObjectId(id)
    }

    /// Finds segments that reference the given object.
    /// Returns non if the object wasn't found or if the tracked object isn't found in the relations
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

    /// Returns an immutable iterator over all the objects.
    pub fn iter(&self) -> impl Iterator<Item = (GlobalObjectId, &Object)> {
        self.objects
            .iter()
            .enumerate()
            .map(|(id, object)| (GlobalObjectId(id), object))
    }

    /// Returns the resolved symbol for the given object.
    ///
    /// If the object is not resolved or is not referenced, returns `None`.
    pub fn get_state(&self, id: GlobalObjectId) -> Option<ObjectState> {
        Some(self.objects.get(id.0)?.state)
    }
}
