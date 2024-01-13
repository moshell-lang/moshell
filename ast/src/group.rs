use src_macros::segment_holder;

use crate::Expr;

/// A block expression `{ ... }`
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    //underlying expressions
    pub expressions: Vec<Expr<'a>>,
}

/// A parenthesis expression `( ... )`
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct Parenthesis<'a> {
    //underlying expression
    pub expression: Box<Expr<'a>>,
}

/// A subshell expression `( ... )`
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct Subshell<'a> {
    //underlying expressions
    pub expressions: Vec<Expr<'a>>,
}
