use crate::name::Name;
use context::source::{OwnedSource, Source};
use std::collections::HashMap;
use std::path::PathBuf;
use ast::Expr;


/// An importer is responsible for importing an AST from a given name
/// The user of the analyzer must provide its own implementation
pub trait ASTImporter {
    /// Gets a source reference from the given import name.
    fn import(&mut self, name: &Name) -> Option<Expr<'static>>;
}

/// An importer that reads files from a given root directory.
pub struct FileASTImporter {
    /// The root directory from which to read files.
    root: PathBuf,

    /// A cache that holds the source code of files that have already been read.
    ///
    /// The main purpose the importer is to be the owner of the source, so it should be
    /// assumed that entries are never removed from the cache.
    cache: HashMap<Name, OwnedSource>,
}

pub struct StaticImporter<'a, P> where P: Fn(Source<'a>) -> Expr<'a> {
    ast_supplier: P,
    sources: HashMap<Name, Source<'a>>,
}

impl<'a, P> StaticImporter<'a, P> where P: Fn(Source<'a>) -> Expr<'a> {
    pub fn new<const N: usize>(sources: [(Name, Source<'a>); N], ast_supplier: P) -> Self {
        Self {
            ast_supplier,
            sources: HashMap::from(sources),
        }
    }
}

impl<'a, P> ASTImporter for StaticImporter<'a, P> where P: Fn(Source<'a>) -> Expr<'a> {
    fn import(&mut self, name: &Name) -> Option<Expr<'static>> {
        let expr = self.sources
            .get(name)
            .map(|src| (self.ast_supplier)(*src));
        unsafe { std::mem::transmute::<Option<Expr<'a>>, Option<Expr<'static>>>(expr) }
    }
}
