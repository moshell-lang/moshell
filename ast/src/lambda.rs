use dbg_pls::DebugPls;

use src_macros::segment_holder;

use crate::variable::TypedVariable;
use crate::Expr;

///A Lambda definition structure
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct LambdaDef<'a> {
    ///lambda's arguments (with optional types)
    pub args: Vec<TypedVariable<'a>>,
    ///the expression's body
    pub body: Box<Expr<'a>>,
}
