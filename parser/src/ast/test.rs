use crate::ast::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct Test<'a> {
    pub(crate) expression: Box<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Not<'a> {
    pub(crate) right: Box<Expr<'a>>,
}
