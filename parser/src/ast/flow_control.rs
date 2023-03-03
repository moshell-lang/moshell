use crate::ast::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct If<'a> {
    pub condition: Box<Expr<'a>>,
    pub success_branch: Box<Expr<'a>>,
    pub fail_branch: Box<Option<Expr<'a>>>
}

