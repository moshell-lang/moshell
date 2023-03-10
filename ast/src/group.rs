use crate::Expr;
use dbg_pls::DebugPls;

/// A block expression `{ ... }`
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Block<'a> {
    //underlying expressions
    pub expressions: Vec<Expr<'a>>,
}

/// A parenthesis expression `( ... )`
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Parenthesis<'a> {
    //underlying expression
    pub expression: Box<Expr<'a>>,
}

/// A subshell expression `( ... )`
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Subshell<'a> {
    //underlying expressions
    pub expressions: Vec<Expr<'a>>,
}
