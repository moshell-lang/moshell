use src_macros::segment_holder;

use crate::Expr;

/// A block expression `{ ... }`
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    //underlying expressions
    pub expressions: Vec<Expr>,
}

/// A parenthesis expression `( ... )`
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct Parenthesis {
    //underlying expression
    pub expression: Box<Expr>,
}

/// A subshell expression `( ... )`
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct Subshell {
    //underlying expressions
    pub expressions: Vec<Expr>,
}
