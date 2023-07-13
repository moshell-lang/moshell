use ast::range::NumericRange;
use ast::Expr;
use lexer::token::TokenType;

use crate::moves::of_type;
use crate::parser::{ParseResult, Parser};

pub trait RangeAspect<'a> {
    /// Parses a range or an iterable variable expression.
    fn parse_range(&mut self, start: Expr<'a>) -> ParseResult<NumericRange<'a>>;
}

impl<'a> RangeAspect<'a> for Parser<'a> {
    /// Parses a range or an iterable variable expression.
    fn parse_range(&mut self, start: Expr<'a>) -> ParseResult<NumericRange<'a>> {
        // Read the second bound of the range
        let upper_inclusive = self.cursor.advance(of_type(TokenType::Equal)).is_some();
        let end = self.next_value()?;

        // Read the step of the range if it exists
        let mut step: Option<Expr<'a>> = None;
        if self.cursor.advance(of_type(TokenType::DotDot)).is_some() {
            step = Some(self.next_value()?);
        }

        Ok(NumericRange {
            start: Box::new(start),
            end: Box::new(end),
            step: step.map(Box::new),
            upper_inclusive,
        })
    }
}
