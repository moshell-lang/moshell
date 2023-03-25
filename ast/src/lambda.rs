use dbg_pls::DebugPls;
use crate::variable::TypedVariable;
use crate::Expr;

///A Lambda definition structure
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct LambdaDef<'a> {
    ///lambda's arguments (with optional types)
    pub args: Vec<TypedVariable<'a>>,
    ///the expression's body
    pub body: Box<Expr<'a>>,
}
