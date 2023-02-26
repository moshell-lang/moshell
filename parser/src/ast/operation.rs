use enum_assoc::Assoc;
use lexer::token::TokenType;

use crate::ast::Expr;
use crate::ast::operation::BinaryOperator::*;

pub const ARITHMETICS: &[BinaryOperator] = &[Plus, Minus, Times, Divide, Modulo];
pub const COMPARISONS: &[BinaryOperator] = &[EqualEqual, NotEqual, Less, LessEqual, Greater, GreaterEqual];
pub const BOOLEANS: &[BinaryOperator] = &[And, Or];


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
#[func(pub const fn token(& self) -> TokenType)]
pub enum BinaryOperator {
    /// The '&&' operator.
    #[assoc(priority = -2)]
    #[assoc(token = TokenType::And)]
    And,
    /// The '||' operator.
    #[assoc(priority = -2)]
    #[assoc(token = TokenType::Or)]
    Or,

    /// The `==` operator.
    #[assoc(priority = -1)]
    #[assoc(token = TokenType::EqualEqual)]
    EqualEqual,
    /// The `!=` operator.
    #[assoc(priority = -1)]
    #[assoc(token = TokenType::NotEqual)]
    NotEqual,
    /// The `<` operator.
    #[assoc(priority = -1)]
    #[assoc(token = TokenType::Less)]
    Less,
    /// The `<=` operator.
    #[assoc(priority = -1)]
    #[assoc(token = TokenType::LessEqual)]
    LessEqual,
    /// The `>` operator.
    #[assoc(priority = -1)]
    #[assoc(token = TokenType::Greater)]
    Greater,
    /// The `>=` operator.
    #[assoc(priority = -1)]
    #[assoc(token = TokenType::GreaterEqual)]
    GreaterEqual,

    /// The `+` operator.
    #[assoc(priority = 0)]
    #[assoc(token = TokenType::Plus)]
    Plus,
    /// The `-` operator.
    #[assoc(priority = 0)]
    #[assoc(token = TokenType::Minus)]
    Minus,
    /// The `*` operator.
    #[assoc(priority = 1)]
    #[assoc(token = TokenType::Star)]
    Times,
    /// The `/` operator.
    #[assoc(priority = 1)]
    #[assoc(token = TokenType::Slash)]
    Divide,
    /// The `%` operator.
    #[assoc(priority = 1)]
    #[assoc(token = TokenType::Percent)]
    Modulo,
}

impl BinaryOperator {
    ///Convert a TokenType to a BinaryOperator;
    /// This function panics if the given token type is not translatable to a BinaryOperator.
    pub fn convert_bin_operator(token_type: TokenType) -> Result<BinaryOperator, &'static str> {
        match token_type {
            TokenType::And => Ok(And),
            TokenType::Or => Ok(Or),

            TokenType::EqualEqual => Ok(EqualEqual),
            TokenType::NotEqual => Ok(NotEqual),
            TokenType::Less => Ok(Less),
            TokenType::LessEqual => Ok(LessEqual),
            TokenType::Greater => Ok(Greater),
            TokenType::GreaterEqual => Ok(GreaterEqual),

            TokenType::Plus => Ok(Plus),
            TokenType::Minus => Ok(Minus),
            TokenType::Star => Ok(Times),
            TokenType::Slash => Ok(Divide),
            TokenType::Percent => Ok(Modulo),
            _ => Err("unexpected non-binary operator token.")
        }
    }
}
