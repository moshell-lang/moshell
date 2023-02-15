use crate::ast::Expr;

/// A block statement `{ ... }`
pub struct Block<'a> {
    pub exprs: Vec<Expr<'a>>
}
//TODO add if / for / while / match statements