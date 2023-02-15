use lexer::token::TokenType::EndOfFile;
use lexer::token::{Token, TokenType};

use crate::parser::{ParseError, ParseResult, Parser};

///Defines all basic operations a parser can do.
pub trait BaseParser<'a> {
    ///advance if the next token is the expected one.
    ///return true of we advanced, false instead.
    fn meet_token(&mut self, expected: TokenType) -> bool;
    ///advance if the next token is the expected one.
    ///return Some containing the next token if it's of the expected type
    /// None instead.
    fn match_token(&mut self, expected: TokenType) -> Option<Token<'a>>;

    fn match_token_space_aware(&mut self, expected: TokenType) -> Option<Token<'a>>;
    ///advance if the next token is the expected one.
    /// return Ok(Token<'a>) where the value is the next token, ParseError if the next token is not
    /// of the expected type
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
    ///advance and returns next token.
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

    fn expect_separated_token(
        &mut self,
        expected: TokenType,
        message: &str,
    ) -> ParseResult<Token<'a>> {
        if self.peek_token_space_aware().token_type != TokenType::Space {
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

    fn peek_token_space_aware(&self) -> Token<'a> {
        self.tokens
            .get(self.current)
            .cloned()
            .unwrap_or_else(|| Token::new(EndOfFile, ""))
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
