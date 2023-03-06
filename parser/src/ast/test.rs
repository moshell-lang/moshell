use crate::ast::Expr;
use dbg_pls::DebugPls;

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Test<'a> {
    pub(crate) expression: Box<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Not<'a> {
    pub(crate) right: Box<Expr<'a>>,
}
