use crate::ast::Expr;
use crate::moves::{blanks, of_type};
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType::While;

///a parser aspect for loops and while expressions
pub trait LoopAspect<'a> {
    fn parse_while(&mut self) -> ParseResult<Expr<'a>>;
    fn parse_loop(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> LoopAspect<'a> for Parser<'a> {
    fn parse_while(&mut self) -> ParseResult<Expr<'a>> {
        self.cursor.force(
            of_type(While),
            "expected 'while' at start of while expression",
        )?;
        self.cursor.advance(blanks());
        // let condition = self.expression_statement();
        // let condition =
        todo!()
    }

    fn parse_loop(&mut self) -> ParseResult<Expr<'a>> {
        todo!()
    }
}
