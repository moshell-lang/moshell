use lexer::token::TokenType;

use crate::ast::callable::Call;
use crate::ast::Expr;
use crate::moves::of_type;
use crate::parser::{ParseResult, Parser};

pub trait CallParser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> CallParser<'a> for Parser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let mut args = vec![self.expression()?];
        let mut cursor = self.cursor;
        while !cursor.is_at_end() && cursor.advance(of_type(TokenType::NewLine)).is_none() {
            args.push(self.expression()?);
        }

        Ok(Expr::Call(Call { arguments: args }))
    }
}
