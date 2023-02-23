
use crate::ast::Expr;

/// A block expression `{ ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    pub expressions: Vec<Expr<'a>>,
}

/// A parenthesis expression `( ... )`
#[derive(Debug, Clone, PartialEq)]
pub struct Parenthesis<'a> {
    pub expressions: Vec<Expr<'a>>
}
