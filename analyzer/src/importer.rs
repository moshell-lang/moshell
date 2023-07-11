use std::collections::HashMap;

use ast::Expr;
use context::source::Source;

use crate::name::Name;

/// An importer is responsible for importing an AST from a given name
/// The user of the analyzer must provide its own implementation
pub trait ASTImporter<'a> {
    /// Gets a source reference from the given import name.
    /// Returning None if the importer encountered any error (IO, parsing errors etc).
    /// It's up to the implementation to handle those errors and report them to the user.
    fn import(&mut self, name: &Name) -> Option<Expr<'a>>;
}

/// An importer with predefined sources.
/// This importer implementation should only be used for tests.
pub struct StaticImporter<'a, F>
where
    F: Fn(Source<'a>) -> Expr<'a>,
{
    ast_factory: F,
    sources: HashMap<Name, Source<'a>>,
}

impl<'a, P> StaticImporter<'a, P>
where
    P: Fn(Source<'a>) -> Expr<'a>,
{
    pub fn new<const N: usize>(sources: [(Name, Source<'a>); N], ast_supplier: P) -> Self {
        Self {
            ast_factory: ast_supplier,
            sources: HashMap::from(sources),
        }
    }
}

impl<'a, P> ASTImporter<'a> for StaticImporter<'a, P>
where
    P: Fn(Source<'a>) -> Expr<'a>,
{
    fn import(&mut self, name: &Name) -> Option<Expr<'a>> {
        self.sources.get(name).map(|src| (self.ast_factory)(*src))
    }
}
