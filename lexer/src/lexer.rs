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
    fn variable_and_initializer() {
        let tokens = lex("var x = 1+2");
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenType::Var, "var"),
                Token::new(TokenType::Space, " "),
                Token::new(TokenType::Identifier, "x"),
                Token::new(TokenType::Space, " "),
                Token::new(TokenType::Equal, "="),
                Token::new(TokenType::Space, " "),
                Token::new(TokenType::IntLiteral, "1"),
                Token::new(TokenType::Plus, "+"),
                Token::new(TokenType::IntLiteral, "2"),
            ]
        );
    }

    #[test]
    fn relative_path() {
        let tokens = lex("cd ./some/path");
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenType::Identifier, "cd"),
                Token::new(TokenType::Space, " "),
                Token::new(TokenType::Identifier, "./some/path"),
            ]
        );
    }

    #[test]
    fn quoted_arithmetic() {
        let tokens = lex("echo \"Result $((1+2*3))\"");
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenType::Identifier, "echo"),
                Token::new(TokenType::Space, " "),
                Token::new(TokenType::DoubleQuote, "\""),
                Token::new(TokenType::Identifier, "Result"),
                Token::new(TokenType::Space, " "),
                Token::new(TokenType::Dollar, "$"),
                Token::new(TokenType::RoundedLeftBracket, "("),
                Token::new(TokenType::RoundedLeftBracket, "("),
                Token::new(TokenType::IntLiteral, "1"),
                Token::new(TokenType::Plus, "+"),
                Token::new(TokenType::IntLiteral, "2"),
                Token::new(TokenType::Times, "*"),
                Token::new(TokenType::IntLiteral, "3"),
                Token::new(TokenType::RoundedRightBracket, ")"),
                Token::new(TokenType::RoundedRightBracket, ")"),
                Token::new(TokenType::DoubleQuote, "\""),
            ]
        );
    }

    #[test]
    fn escaped_quote() {
        let tokens = lex("cat filename\\`with\\`spaces");
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenType::Identifier, "cat"),
                Token::new(TokenType::Space, " "),
                Token::new(TokenType::Identifier, "filename\\`with\\`spaces"),
            ]
        );
    }
}
