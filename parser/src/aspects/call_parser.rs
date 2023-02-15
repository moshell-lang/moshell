use lexer::token::TokenType;

use crate::aspects::base_parser::BaseParser;
use crate::ast::callable::Call;
use crate::ast::Expr;
use crate::parser::{ParseResult, Parser};

pub trait CallParser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> CallParser<'a> for Parser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let mut args = vec![self.expression()?];
        while !self.is_at_end() && !self.meet_token(TokenType::NewLine) {
            args.push(self.expression()?);
        }

        Ok(Expr::Call(Call { arguments: args }))
    }
}
