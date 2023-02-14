use crate::ast::Expr;

/// A binary operation between two expressions.
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperation<'a> {
    /// The left-hand side of the operation.
    pub left: Box<Expr<'a>>,
    /// The operator of the operation.
    pub op: BinaryOperator,
    /// The right-hand side of the operation.
    pub right: Box<Expr<'a>>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinaryOperator {
    /// The `==` operator.
    EqualEqual,
    /// The `!=` operator.
    NotEqual,
    /// The `<` operator.
    Less,
    /// The `<=` operator.
    LessEqual,
    /// The `>` operator.
    Greater,
    /// The `>=` operator.
    GreaterEqual,
    /// The `+` operator.
    Plus,
    /// The `-` operator.
    Minus,
    /// The `*` operator.
    Star,
    /// The `/` operator.
    Slash,
    /// The `%` operator.
    Percent,
}
