use crate::ast::Expr;

/// a test (`[ ... ]`) expression
#[derive(Debug, Clone, PartialEq)]
pub struct Test<'a> {
    ///expression present between brackets
    pub(crate) expression: Box<Expr<'a>>
}

///a not (`! ..`) expression
#[derive(Debug, Clone, PartialEq)]
pub struct Not<'a> {
    ///the expression after `!`
    pub(crate) underlying: Box<Expr<'a>>
}