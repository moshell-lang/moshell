use dbg_pls::DebugPls;

use src_macros::segment_holder;

use crate::Expr;

/// a test (`[ ... ]`) expression
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Test<'a> {
    ///expression present between brackets
    pub expression: Box<Expr<'a>>,
}
