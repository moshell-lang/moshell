use enum_assoc::Assoc;

use crate::ast::Expr;


/// An arithmetic operation between two expressions.
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperation<'a> {
    /// The left-hand side of the operation.
    pub left: Box<Expr<'a>>,
    /// The operator of the operation.
    pub op: BinaryOperator,
    /// The right-hand side of the operation.
    pub right: Box<Expr<'a>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Assoc)]
#[func(pub const fn priority(& self) -> i8)]
pub enum BinaryOperator {
    /// The '&&' operator.
    #[assoc(priority = -2)]
    And,
    /// The '||' operator.
    #[assoc(priority = -2)]
    Or,

    /// The `==` operator.
    #[assoc(priority = -1)]
    EqualEqual,
    /// The `!=` operator.
    #[assoc(priority = -1)]
    NotEqual,
    /// The `<` operator.
    #[assoc(priority = -1)]
    Less,
    /// The `<=` operator.
    #[assoc(priority = -1)]
    LessEqual,
    /// The `>` operator.
    #[assoc(priority = -1)]
    Greater,
    /// The `>=` operator.
    #[assoc(priority = -1)]
    GreaterEqual,

    /// The `+` operator.
    #[assoc(priority = 0)]
    Plus,
    /// The `-` operator.
    #[assoc(priority = 0)]
    Minus,
    /// The `*` operator.
    #[assoc(priority = 1)]
    Times,
    /// The `/` operator.
    #[assoc(priority = 1)]
    Divide,
    /// The `%` operator.
    #[assoc(priority = 1)]
    Modulo,
}
