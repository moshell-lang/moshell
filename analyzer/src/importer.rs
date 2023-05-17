use crate::name::Name;
use context::source::{Source};
use std::collections::HashMap;
use ast::Expr;

/// An importer is responsible for importing an AST from a given name
/// The user of the analyzer must provide its own implementation
pub trait ASTImporter<'a> {
    /// Gets a source reference from the given import name.
    fn import(&mut self, name: &Name) -> Option<Expr<'a>>;
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

impl<'a, P> ASTImporter<'a> for StaticImporter<'a, P> where P: Fn(Source<'a>) -> Expr<'a> {
    fn import(&mut self, name: &Name) -> Option<Expr<'a>> {
        self.sources
            .get(name)
            .map(|src| (self.ast_supplier)(*src))
    }
}