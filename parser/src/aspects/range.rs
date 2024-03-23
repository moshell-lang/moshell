use crate::aspects::binary_operation::infix_precedence;
use ast::range::{NumericRange, Subscript};
use ast::Expr;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType;
use std::num::NonZeroU8;

use crate::moves::of_type;
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    /// Parses a range or an iterable variable expression.
    pub(crate) fn parse_range(&mut self, start: Expr) -> ParseResult<NumericRange> {
        // Could use a constant when `.expect()` becomes a const fn
        let precedence = NonZeroU8::new(infix_precedence(TokenType::DotDot) + 1)
            .expect("Precedence should be non-zero");
        // Read the second bound of the range
        let upper_inclusive = self.cursor.advance(of_type(TokenType::Equal)).is_some();
        let end = self.value_precedence(precedence)?;

        // Read the step of the range if it exists
        let mut step: Option<Expr> = None;
        if self.cursor.advance(of_type(TokenType::DotDot)).is_some() {
            step = Some(self.value_precedence(precedence)?);
        }

        Ok(NumericRange {
            start: Box::new(start),
            end: Box::new(end),
            step: step.map(Box::new),
            upper_inclusive,
        })
    }

    /// Parses a subscript expression.
    pub(crate) fn parse_subscript(&mut self, target: Expr) -> ParseResult<Subscript> {
        self.cursor.force(
            of_type(TokenType::SquaredLeftBracket),
            "Expected '[' after subscript target",
        )?;
        let value = self.value()?;
        let closing_bracket = self.cursor.force(
            of_type(TokenType::SquaredRightBracket),
            "Expected ']' after subscript target",
        )?;
        let segment = target.segment().start..closing_bracket.span.end;
        Ok(Subscript {
            target: Box::new(target),
            index: Box::new(value),
            segment,
        })
    }
}
