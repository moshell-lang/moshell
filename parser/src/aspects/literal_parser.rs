use lexer::token::Token;

use crate::ast::{Expr, Literal};
use crate::parser::{ParseResult, Parser};

pub(crate) trait LiteralParser<'a> {
    fn literal(&mut self, token: Token<'a>) -> ParseResult<Expr<'a>>;
}

impl<'a> LiteralParser<'a> for Parser<'a> {
    fn literal(&mut self, token: Token<'a>) -> ParseResult<Expr<'a>> {
        Ok(Expr::Literal(Literal { value: token }))
    }
}
