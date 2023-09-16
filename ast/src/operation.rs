use dbg_pls::DebugPls;

use context::source::{SourceSegment, SourceSegmentHolder};
use lexer::token::TokenType;
use src_macros::segment_holder;

use crate::variable::AssignOperator;
use crate::Expr;

/// A prefix unary operation.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct UnaryOperation<'a> {
    /// The operator of the operation.
    pub op: UnaryOperator,

    /// The expression the operator is applied to.
    pub expr: Box<Expr<'a>>,
}

/// A prefix unary operator.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum UnaryOperator {
    /// The `!` operator.
    Not,
    /// The `-` operator.
    Negate,
}

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

#[derive(Debug, Copy, Clone, PartialEq, DebugPls)]
pub enum BinaryOperator {
    /// The '&&' operator.
    And,
    /// The '||' operator.
    Or,

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
    Times,
    /// The `/` operator.
    Divide,
    /// The `%` operator.
    Modulo,
}

impl TryFrom<TokenType> for BinaryOperator {
    type Error = &'static str;

    /// Convert a TokenType to a BinaryOperator
    fn try_from(token_type: TokenType) -> Result<Self, Self::Error> {
        use crate::operation::BinaryOperator::*;
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

impl TryFrom<AssignOperator> for BinaryOperator {
    type Error = ();

    fn try_from(value: AssignOperator) -> Result<Self, Self::Error> {
        Ok(match value {
            AssignOperator::Assign => return Err(()),
            AssignOperator::Increment => Self::Plus,
            AssignOperator::Decrement => Self::Minus,
            AssignOperator::Multiply => Self::Times,
            AssignOperator::Divide => Self::Divide,
            AssignOperator::Remainder => Self::Modulo,
        })
    }
}
