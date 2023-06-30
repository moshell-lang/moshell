use std::collections::HashMap;

use ast::Expr;
use context::source::{ContentId, Source};

use crate::name::Name;

/// An imported expression that is bound to a content identifier.
#[derive(Debug, Clone, PartialEq)]
pub struct Imported<'a> {
    /// The content identifier from which the expression was imported.
    pub content: ContentId,

    /// The imported expression.
    pub expr: Expr<'a>,
}

/// An vague error type that indicates that something went wrong while importing.
#[derive(Debug, PartialEq)]
pub struct ImportError;

/// Import an abstract syntax tree from a given name.
pub trait ASTImporter<'a> {
    /// Gets an expression from the given import name.
    ///
    /// This method should return `Ok(None)` if a source with the given name
    /// could not be found. If the source could be found, but for any reason
    /// could not be retrieved (because of IO or parsing errors), this method
    /// should return `Err(())`. Implementers of this traits may expose the
    /// actual error types.
    fn import(&mut self, name: &Name) -> Result<Option<Imported<'a>>, ImportError>;
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
    fn import(&mut self, name: &Name) -> Result<Option<Imported<'a>>, ImportError> {
        let ast = self.sources.get(name).map(|src| (self.ast_factory)(*src));
        Ok(ast.map(|expr| Imported {
            content: ContentId(0),
            expr,
        }))
    }
}
