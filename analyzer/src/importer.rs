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

/// The outcome when trying to get an expression from a [`Name`].
#[derive(Debug, PartialEq)]
pub enum ImportResult<'a> {
    /// The import was successful and can be used.
    Success(Imported<'a>),

    /// The source could not be found. Another name may be tried.
    NotFound,

    /// The source has been found but could not be retrieved.
    ///
    /// This error is fatal and should not be ignored.
    Failure,
}

/// Import an abstract syntax tree from a given name.
pub trait ASTImporter<'a> {
    /// Gets an expression from the given import name.
    ///
    /// This method should return [`ImportResult::NotFound`] if a source with
    /// the given name could not be found. If the source could be found, but for
    /// any reason could not be retrieved (because of IO or parsing errors),
    /// this method should return [`ImportResult::Failure`]. Implementers of
    /// this trait may expose the actual error types.
    fn import(&mut self, name: &Name) -> ImportResult<'a>;
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
    fn import(&mut self, name: &Name) -> ImportResult<'a> {
        let ast = self.sources.get(name).map(|src| (self.ast_factory)(*src));
        ast.map(|expr| Imported {
            content: ContentId(0),
            expr,
        })
        .into()
    }
}

impl<'a> From<Option<Imported<'a>>> for ImportResult<'a> {
    fn from(opt: Option<Imported<'a>>) -> Self {
        match opt {
            Some(imported) => ImportResult::Success(imported),
            None => ImportResult::NotFound,
        }
    }
}
