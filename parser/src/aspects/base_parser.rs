use lexer::token::TokenType::EndOfFile;
use lexer::token::{Token, TokenType};

use crate::parser::{ParseError, ParseResult, Parser};

///Defines all basic operations a parser can do.
pub trait BaseParser<'a> {
    fn meet_token(&mut self, expected: TokenType) -> bool;
    fn match_token(&mut self, expected: TokenType) -> Option<Token<'a>>;
    fn expect_token(&mut self, expected: TokenType, message: &str) -> ParseResult<Token<'a>>;
    fn expect_separated_token(
        &mut self,
        expected: TokenType,
        message: &str,
    ) -> ParseResult<Token<'a>>;
    fn peek_token(&self) -> Token<'a>;
    fn next_token(&mut self) -> ParseResult<Token<'a>>;
    fn next_token_space_aware(&mut self) -> ParseResult<Token<'a>>;
    fn is_at_end(&self) -> bool;
    fn expected(&self, message: &str) -> ParseResult<Token<'a>>;
    fn mk_parse_error(&self, message: impl Into<String>) -> ParseError;
}

impl<'a> BaseParser<'a> for Parser<'a> {
    fn meet_token(&mut self, expected: TokenType) -> bool {
        self.match_token(expected).is_some()
    }

    fn match_token(&mut self, expected: TokenType) -> Option<Token<'a>> {
        let mut idx = self.current;
        while let Some(token) = self.tokens.get(idx) {
            idx += 1;
            if token.token_type != TokenType::Space && token.token_type == expected {
                self.current = idx;
                return Some(token.clone());
            }
        }
        None
    }

    fn expect_token(&mut self, expected: TokenType, message: &str) -> ParseResult<Token<'a>> {
        self.match_token(expected)
            .ok_or_else(|| self.mk_parse_error(message))
    }

    fn expect_separated_token(
        &mut self,
        expected: TokenType,
        message: &str,
    ) -> ParseResult<Token<'a>> {
        if self
            .tokens
            .get(self.current)
            .map(|token| token.token_type)
            .unwrap_or(EndOfFile)
            != TokenType::Space
        {
            self.expected("Excepted a space")?;
        }
        self.expect_token(expected, message)
    }

    fn peek_token(&self) -> Token<'a> {
        let mut idx = self.current;
        while let Some(token) = self.tokens.get(idx) {
            idx += 1;
            if token.token_type != TokenType::Space {
                return token.clone();
            }
        }
        return Token::new(EndOfFile, "");
    }

    fn next_token(&mut self) -> ParseResult<Token<'a>> {
        while let Some(token) = self.tokens.get(self.current) {
            self.current += 1;
            if token.token_type != TokenType::Space {
                return Ok(token.clone());
            }
        }
        self.expected("Unexpected end of file.")
    }

    fn next_token_space_aware(&mut self) -> ParseResult<Token<'a>> {
        if let Some(token) = self.tokens.get(self.current) {
            self.current += 1;
            return Ok(token.clone());
        }
        self.expected("Unexpected end of file.")
    }

    fn is_at_end(&self) -> bool {
        self.peek_token().token_type == EndOfFile
    }

    fn expected(&self, message: &str) -> ParseResult<Token<'a>> {
        Err(self.mk_parse_error(message))
    }

    fn mk_parse_error(&self, message: impl Into<String>) -> ParseError {
        ParseError {
            message: message.into(),
            //actual: self.peek_token().clone(),
        }
    }
}
