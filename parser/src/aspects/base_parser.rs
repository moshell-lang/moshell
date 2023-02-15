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
    ///advance if the next token is the expected one.
    /// return Ok(Token<'a>) where the value is the next token, ParseError if the next token is not
    /// of the expected type
    fn expect_token(&mut self, expected: TokenType, message: &str) -> ParseResult<Token<'a>>;
    ///returns the next token without advancing
    fn peek_token(&self) -> Token<'a>;
    ///advance and returns next token.
    fn next_token(&mut self) -> ParseResult<Token<'a>>;
    ///return true if this parser is at the end of the Token stream (hit EOF)
    fn is_at_end(&self) -> bool;
    fn expected(&self, message: &str) -> ParseResult<Token<'a>>;
    fn mk_parse_error(&self, message: impl Into<String>) -> ParseError;
}

impl<'a> BaseParser<'a> for Parser<'a> {
    fn meet_token(&mut self, expected: TokenType) -> bool {
        let token = self.peek_token();
        if token.token_type == expected {
            self.current += 1;
            return true;
        }
        false
    }

    fn match_token(&mut self, expected: TokenType) -> Option<Token<'a>> {
        let token = self.peek_token();
        if token.token_type == expected {
            self.current += 1;
            return Some(token.clone());
        }
        None
    }

    fn expect_token(&mut self, expected: TokenType, message: &str) -> ParseResult<Token<'a>> {
        self.match_token(expected)
            .ok_or_else(|| self.mk_parse_error(message))
    }

    fn peek_token(&self) -> Token<'a> {
        self.tokens
            .get(self.current)
            .cloned()
            .unwrap_or(Token::new(EndOfFile, ""))
    }

    fn next_token(&mut self) -> ParseResult<Token<'a>> {
        let token = self.peek_token();
        if token.token_type != EndOfFile {
            self.current += 1;
            Ok(token.clone())
        } else {
            self.expected("Unexpected end of file.")
        }
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
