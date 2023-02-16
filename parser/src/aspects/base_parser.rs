use lexer::token::TokenType::EndOfFile;
use lexer::token::{Token, TokenType};

use crate::parser::{ParseError, ParseResult, Parser};

///Defines all basic operations a parser can do.
pub trait BaseParser<'a> {
    fn meet_token(&mut self, expected: TokenType) -> bool;
    fn match_token(&mut self, expected: TokenType) -> Option<Token<'a>>;
    fn match_token_space_aware(&mut self, expected: TokenType) -> Option<Token<'a>>;
    fn expect_token(&mut self, expected: TokenType, message: &str) -> ParseResult<Token<'a>>;
    fn expect_token_space_aware(
        &mut self,
        expected: TokenType,
        message: &str,
    ) -> ParseResult<Token<'a>>;
    fn expect_separated_token(
        &mut self,
        expected: TokenType,
        message: &str,
    ) -> ParseResult<Token<'a>>;
    fn peek_token(&self) -> Token<'a>;
    fn peek_token_space_aware(&self) -> Token<'a>;
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
            if token.token_type == TokenType::Space {
                continue;
            }
            return if token.token_type == expected {
                self.current = idx;
                Some(token.clone())
            } else {
                None
            };
        }
        None
    }

    fn match_token_space_aware(&mut self, expected: TokenType) -> Option<Token<'a>> {
        self.tokens.get(self.current).and_then(|token| {
            if token.token_type == expected {
                self.current += 1;
                Some(token.clone())
            } else {
                None
            }
        })
    }

    fn expect_token(&mut self, expected: TokenType, message: &str) -> ParseResult<Token<'a>> {
        self.match_token(expected)
            .ok_or_else(|| self.mk_parse_error(message))
    }

    fn expect_token_space_aware(
        &mut self,
        expected: TokenType,
        message: &str,
    ) -> ParseResult<Token<'a>> {
        self.match_token_space_aware(expected)
            .ok_or_else(|| self.mk_parse_error(message))
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