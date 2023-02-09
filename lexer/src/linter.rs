#![allow(dead_code)]

use logos::Logos;

use crate::token::*;

#[derive(Debug, PartialEq)]
pub struct LexError {
    pub message: String,
    pub offset: usize,
}


pub fn lex<'a>(inp: &str) -> Vec<&'a dyn Token> {
    let mut lexer = TokenType::lexer(inp);

    let mut line = 1;

    let mut tokens: Vec<TokenType> = vec![];
    while let Some(token_type) = lexer.next() {
        let value = match token_type {
            TokenType::Identifier(value) => Some(value),
            TokenType::IntLiteral(value) => Some(value),
            TokenType::FloatLiteral(value) => Some(value),
            TokenType::AppendRedirect(kind) => Some(&kind.to_string()[..]),
            TokenType::Redirect(kind) => Some(&kind.to_string()[..]),
            _ => None,
        };
        if token_type == TokenType::NewLine {
            line += 1
        }

        let span = lexer.span();
        let col = span.start;

        tokens.push(&TokenStruct::new(token_type, value, line, col));
    }
    todo!();
}

