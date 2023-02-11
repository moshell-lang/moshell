#![allow(dead_code)]

use logos::Logos;

use crate::token::*;

#[derive(Debug, PartialEq)]
pub struct LexError {
    pub message: String,
    pub offset: usize,
}


pub fn lex<'a>(inp: &str) -> Vec<&'a Token> {
    let mut lexer = TokenType::lexer(inp);

    let mut line = 1;

    let mut tokens: Vec<Token<'a>> = vec![];
    while let Some(token_type) = lexer.next() {
        let value = match token_type {
            TokenType::Identifier(value) => Some(value.to_string()),
            TokenType::IntLiteral(value) => Some(value.to_string()),
            TokenType::FloatLiteral(value) => Some(value.to_string()),
            TokenType::AppendRedirect(kind) => Some(kind.to_string()),
            TokenType::Redirect(kind) => Some(kind.to_string()),
            _ => None,
        };
        if token_type == TokenType::NewLine {
            line += 1
        }
        let span = lexer.span();
        let col = span.start;

        tokens.push(Token{token_type, value, line, column: col});
    }
    todo!();
}

