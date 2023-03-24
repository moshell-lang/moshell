use crate::variable::TypedVariable;
use crate::Expr;

pub struct LambdaDef<'a> {
    pub args: Vec<TypedVariable<'a>>,
    pub body: Box<Expr<'a>>,
}
