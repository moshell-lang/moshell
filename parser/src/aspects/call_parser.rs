use lexer::token::TokenType;

use crate::aspects::base_parser::BaseParser;
use crate::ast::{Call, Expr};
use crate::parser::{ParseResult, Parser};

pub trait CallParser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> CallParser<'a> for Parser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let name = self.expect_token(TokenType::Identifier, "Expected command name.")?;

        let mut args = Vec::new();
        while !self.is_at_end() && !self.meet_token(TokenType::NewLine) {
            args.push(self.expression()?);
        }

        Ok(Expr::Call(Call {
            name: name.clone(),
            arguments: args,
        }))
    }
}
