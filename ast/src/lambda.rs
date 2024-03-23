use src_macros::segment_holder;

use crate::variable::TypedVariable;
use crate::Expr;

///A Lambda definition structure
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct LambdaDef {
    ///lambda's arguments (with optional types)
    pub args: Vec<TypedVariable>,
    ///the expression's body
    pub body: Box<Expr>,
}
