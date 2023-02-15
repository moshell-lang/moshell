use lexer::token::TokenType;

use crate::ast::callable::Call;
use crate::ast::Expr;
use crate::parser::{ParseResult, Parser};

pub trait CallParser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> CallParser<'a> for Parser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let cursor = self.cursor();
        let name = cursor.expect_token(TokenType::Identifier, "Expected command name.")?;

        let mut args = Vec::new();
        while !self.cursor().is_at_end() && !self.cursor().meet_token(TokenType::NewLine) {
            args.push(self.expression()?);
        }

        Ok(Expr::Call(Call {
            name: name.clone(),
            arguments: args,
        }))
    }
}

