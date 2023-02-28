use crate::ast::Expr;

/// A block expression `{ ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    //underlying expressions
    pub expressions: Vec<Expr<'a>>,
}

/// A parenthesis expression `( ... )`
#[derive(Debug, Clone, PartialEq)]
pub struct Parenthesis<'a> {
    //underlying expression
    pub expression: Box<Expr<'a>>,
}

/// A subshell expression `( ... )`
#[derive(Debug, Clone, PartialEq)]
pub struct Subshell<'a> {
    //underlying expressions
    pub expressions: Vec<Expr<'a>>,
}
