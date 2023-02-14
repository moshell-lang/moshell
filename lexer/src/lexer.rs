#![allow(dead_code)]

use logos::Logos;

use crate::token::*;

#[derive(Debug, PartialEq)]
pub struct LexError {
    pub message: String,
    pub offset: usize,
}

pub fn lex(inp: &str) -> Vec<Token> {
    let mut lexer = TokenType::lexer(inp);

    let mut tokens: Vec<Token> = vec![];
    while let Some(token_type) = lexer.next() {
        tokens.push(Token::new(token_type, lexer.slice()))
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn relative_path() {
        let tokens = lex("cd ./some/path");
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenType::Identifier, "cd"),
                Token::new(TokenType::Identifier, "./some/path"),
            ]
        );
    }
}
