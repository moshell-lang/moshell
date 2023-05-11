use crate::name::Name;
use crate::resolver::{SourceObjectId};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnresolvedImports {
    pub source: SourceObjectId,
    pub imports: Vec<UnresolvedImport>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnresolvedImport {
    Symbol { alias: Option<String>, name: Name },
    AllIn(Name)
}

impl UnresolvedImports {

    pub fn new(source: SourceObjectId) -> Self {
        Self {
            source,
            imports: Vec::new()
        }
    }
    
    pub fn with(source: SourceObjectId, imports: Vec<UnresolvedImport>) -> Self {
        Self {
            source,
            imports
        }
    }

    pub fn add_unresolved_import(&mut self, import: UnresolvedImport) {
        self.imports.push(import)
    }
}