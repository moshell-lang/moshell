use crate::variable::TypedVariable;
use crate::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};
use dbg_pls::DebugPls;

///A Lambda definition structure
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct LambdaDef<'a> {
    ///lambda's arguments (with optional types)
    pub args: Vec<TypedVariable<'a>>,
    ///the expression's body
    pub body: Box<Expr<'a>>,
}

impl SourceSegmentHolder for LambdaDef<'_> {
    fn segment(&self) -> SourceSegment {
        self.args.first().unwrap().segment.start..self.body.segment().end
    }
}
