use crate::Expr;
use context::source::SourceSegment;
use dbg_pls::DebugPls;

/// A block expression `{ ... }`
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Block<'a> {
    //underlying expressions
    pub expressions: Vec<Expr<'a>>,

    pub segment: SourceSegment,
}

/// A parenthesis expression `( ... )`
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Parenthesis<'a> {
    //underlying expression
    pub expression: Box<Expr<'a>>,

    pub segment: SourceSegment,
}

/// A subshell expression `( ... )`
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Subshell<'a> {
    //underlying expressions
    pub expressions: Vec<Expr<'a>>,

    pub segment: SourceSegment,
}
