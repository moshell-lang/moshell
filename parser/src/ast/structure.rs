use crate::ast::Expr;
use dbg_pls::DebugPls;

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Construct<'a> {
    pub name: &'a str,
    pub args: Vec<Expr<'a>>,
}
