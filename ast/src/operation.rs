use dbg_pls::DebugPls;
use enum_assoc::Assoc;

use context::source::{SourceSegment, SourceSegmentHolder};
use lexer::token::TokenType;

use crate::operation::BinaryOperator::*;
use crate::Expr;

/// A binary operation between two expressions.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct BinaryOperation<'a> {
    /// The left-hand side of the operation.
    pub left: Box<Expr<'a>>,
    /// The operator of the operation.
    pub op: BinaryOperator,
    /// The right-hand side of the operation.
    pub right: Box<Expr<'a>>,
}

impl SourceSegmentHolder for BinaryOperation<'_> {
    fn segment(&self) -> SourceSegment {
        self.left.segment().start..self.right.segment().end
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Assoc, DebugPls)]
#[func(pub const fn priority(& self) -> i8)]
#[func(pub const fn token(& self) -> TokenType)]
pub enum BinaryOperator {
    /// The '&&' operator.
    #[assoc(priority = -2)]
    #[assoc(token = TokenType::And)]
    And,
    /// The '||' operator.
    #[assoc(priority = -3)]
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

impl TryFrom<TokenType> for BinaryOperator {
    type Error = &'static str;

    /// Convert a TokenType to a BinaryOperator
    fn try_from(token_type: TokenType) -> Result<Self, Self::Error> {
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
            _ => Err("unexpected non-binary operator token."),
        }
    }
}
