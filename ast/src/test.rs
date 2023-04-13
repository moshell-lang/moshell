use crate::Expr;
use dbg_pls::DebugPls;
use src_macros::segment_holder;

/// a test (`[ ... ]`) expression
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Test<'a> {
    ///expression present between brackets
    pub expression: Box<Expr<'a>>,
}

///a not (`! ..`) expression
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Not<'a> {
    ///the expression after `!`
    pub underlying: Box<Expr<'a>>,
}
