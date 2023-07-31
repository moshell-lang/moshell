use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use indexmap::IndexMap;

use context::source::SourceSegment;

use crate::name::Name;
use crate::relations::{ResolvedSymbol, SourceId};

#[derive(Debug, Default)]
pub struct Imports {
    /// Associates a source object with its imports.
    ///
    /// Imports may only be declared at the top level of a source. This lets us track the unresolved imports
    /// per [`crate::environment::Environment`]. If a source is not tracked here, it means that it has no
    /// imports.
    imports: HashMap<SourceId, SourceImports>,
}

impl Imports {
    /// References a new import directive in the given source.
    ///
    /// This directive may be used later to resolve the import.
    pub fn add_unresolved_import(
        &mut self,
        source: SourceId,
        import: UnresolvedImport,
        import_expr: SourceSegment,
    ) -> Option<SourceSegment> {
        let imports = self.imports.entry(source).or_default();
        imports.add_unresolved_import(import, import_expr)
    }

    pub fn get_imports(&self, source: SourceId) -> Option<&SourceImports> {
        self.imports.get(&source)
    }

    pub fn get_imports_mut(&mut self, source: SourceId) -> Option<&mut SourceImports> {
        self.imports.get_mut(&source)
    }

    /// Removes all the imports that were declared at or after the given source.
    pub fn retain_before(&mut self, source: SourceId) {
        self.imports.retain(|id, _| id.0 < source.0);
    }
}

/// The structure that hosts the unresolved and resolved imported symbols of an environment
#[derive(PartialEq, Default)]
pub struct SourceImports {
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
    Env(SourceId),

    /// The import wasn't found
    Dead,
}

impl Debug for SourceImports {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut imported_symbols: Vec<_> = self.imported_symbols.iter().collect();
        imported_symbols.sort_by_key(|(k, _)| *k);
        f.debug_struct("Imports")
            .field("imported_symbols", &imported_symbols)
            .field("unresolved_imports", &self.unresolved_imports)
            .finish()
    }
}

impl SourceImports {
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
