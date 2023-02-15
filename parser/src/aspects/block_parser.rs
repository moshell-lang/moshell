use lexer::token::TokenType;
use crate::aspects::base_parser::BaseParser;
use crate::ast::Expr;
use crate::parser::{Parser, ParseResult};
use crate::ast::statement::Block;

pub trait BlockParser<'a> {
    fn parse_block(&mut self) -> ParseResult<Block<'a>>;
}

impl<'a> BlockParser<'a> for Parser<'a> {
    fn parse_block(&mut self) -> ParseResult<Block<'a>> {
        self.expect_token(TokenType::CurlyLeftBracket, "expected start of block expression")?;
        let mut expressions: Vec<Expr<'a>> = Vec::new();
        loop {
            let expression = self.expression()?;
            expressions.push(expression);
            if self.meet_token(TokenType::CurlyRightBracket) {
                break;
            }
        };
        Ok(Block{
            exprs: expressions
        })
    }
}