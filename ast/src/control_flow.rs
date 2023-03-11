use crate::Expr;
use dbg_pls::DebugPls;

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct If<'a> {
    pub condition: Box<Expr<'a>>,
    pub success_branch: Box<Expr<'a>>,
    pub fail_branch: Option<Box<Expr<'a>>>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct While<'a> {
    pub condition: Box<Expr<'a>>,
    pub body: Box<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Loop<'a> {
    pub body: Box<Expr<'a>>,
}
