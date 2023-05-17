use crate::name::Name;
use std::collections::{HashMap};
use std::hash::{Hash, Hasher};
use indexmap::IndexMap;
use ast::r#use::Import as ImportExpr;

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
    /// A local object, referenced by its index in the [`checker::environment::Environment`] it is defined in.
    Local(ObjectId),

    /// A global object, referenced by its index in the [`Relations`] it is linked to.
    Global(ObjectId),
}

impl From<GlobalObjectId> for Symbol {
    fn from(id: GlobalObjectId) -> Self {
        Symbol::Global(id.0)
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct UnresolvedImports<'a> {
    pub imports: IndexMap<UnresolvedImport, Vec<&'a ImportExpr<'a>>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum UnresolvedImport {
    Symbol { alias: Option<String>, name: Name },
    AllIn(Name),
}

impl<'a> UnresolvedImports<'a> {
    pub fn new(imports: IndexMap<UnresolvedImport, Vec<&'a ImportExpr<'a>>>) -> Self {
        Self { imports }
    }

    pub fn add_unresolved_import(&mut self, import: UnresolvedImport, import_expr: &'a ImportExpr<'a>) {
        self.imports.entry(import)
            .or_insert_with(Vec::new)
            .push(import_expr)
    }
}

/// The resolved information about a symbol.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ResolvedSymbol {
    /// The module where the symbol is defined.
    ///
    /// This is used to route the symbol to the correct environment.
    pub module: SourceObjectId,

    /// The object identifier of the symbol, local to the module.
    pub object_id: ObjectId,
}

impl Hash for ResolvedSymbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.module.hash(state);
        self.object_id.hash(state);
    }
}

impl ResolvedSymbol {
    pub fn new(module: SourceObjectId, object_id: ObjectId) -> Self {
        Self {
            module,
            object_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Object {
    /// The symbol that is being resolved, where it is used.
    pub origin: SourceObjectId,

    /// The link to the resolved symbol.
    pub resolved: Option<ResolvedSymbol>,
}


impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.origin.hash(state);
        self.resolved.hash(state);
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.origin == other.origin && self.resolved == other.resolved
    }
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
pub struct Relations<'a> {
    /// The objects that need resolution that are tracked globally.
    ///
    /// The actual [`String`] -> [`ObjectId`] mapping is left to the [`checker::environment::Environment`].
    /// The reason that the resolution information is lifted out of the environment is that identifiers
    /// binding happens across modules, and an environment cannot guarantee that it will be able to generate
    /// unique identifiers for all the symbols that do not conflicts with the ones from other modules.
    pub objects: Vec<Object>,

    /// Associates a source object with its unresolved imports.
    ///
    /// Imports may only be declared at the top level of a source. This lets us track the unresolved imports
    /// per [`checker::environment::Environment`]. If a source is not tracked here, it means that it has no
    /// imports. This is only used to create find the link between environments and sources, and should not
    /// be used after the resolution is done.
    pub imports: HashMap<SourceObjectId, UnresolvedImports<'a>>,
}

impl<'a> Relations<'a> {
    /// Take the imports
    pub fn take_imports(&mut self) -> HashMap<SourceObjectId, UnresolvedImports<'a>> {
        std::mem::take(&mut self.imports)
    }

    /// References a new import directive in the given source.
    ///
    /// This directive may be used later to resolve the import.
    pub fn add_import(&mut self, source: SourceObjectId, import: UnresolvedImport, import_expr: &'a ImportExpr<'a>) {
        let imports = self
            .imports
            .entry(source)
            .or_insert_with(UnresolvedImports::default);
        imports.add_unresolved_import(import, import_expr)
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
}
