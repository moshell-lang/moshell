use crate::Expr;
use context::source::SourceSegment;
use dbg_pls::DebugPls;
use src_macros::SourceSegmentHolder;

/// a test (`[ ... ]`) expression
#[derive(Debug, Clone, PartialEq, DebugPls, SourceSegmentHolder)]
pub struct Test<'a> {
    ///expression present between brackets
    pub expression: Box<Expr<'a>>,
    pub segment: SourceSegment,
}

///a not (`! ..`) expression
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Not<'a> {
    ///the expression after `!`
    pub underlying: Box<Expr<'a>>,
}
