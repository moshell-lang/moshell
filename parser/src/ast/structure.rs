use crate::ast::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct Construct<'a> {
    pub name: &'a str,
    pub args: Vec<Expr<'a>>,
}
