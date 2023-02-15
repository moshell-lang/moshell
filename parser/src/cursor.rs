use lexer::token::{Token, TokenType};
use lexer::token::TokenType::EndOfFile;
use crate::parser::{ParseError, ParseResult};

pub(crate) struct ParserCursor<'a> {
    tokens: Vec<Token<'a>>,
    pos: usize,
}

impl<'a> ParserCursor<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn meet_token(&mut self, expected: TokenType) -> bool {
        let token = self.peek_token();
        if token.token_type == expected {
            self.pos += 1;
            return true;
        }
        false
    }

    pub fn match_token(&mut self, expected: TokenType) -> Option<Token<'a>> {
        let token = self.peek_token();
        if token.token_type == expected {
            self.pos += 1;
            return Some(token.clone());
        }
        None
    }

    pub fn expect_token(&mut self, expected: TokenType, message: &str) -> ParseResult<Token<'a>> {
        self.match_token(expected)
            .ok_or_else(|| self.mk_parse_error(message))
    }

    pub fn peek_token(&self) -> Token<'a> {
        self.tokens
            .get(self.pos)
            .cloned()
            .unwrap_or(Token::new(EndOfFile, ""))
    }

    pub fn next_token(&mut self) -> ParseResult<Token<'a>> {
        let token = self.peek_token();
        if token.token_type != EndOfFile {
            self.pos += 1;
            Ok(token.clone())
        } else {
            self.expected("Unexpected end of file.")
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.peek_token().token_type == EndOfFile
    }

    pub fn expected(&self, message: &str) -> ParseResult<Token<'a>> {
        Err(self.mk_parse_error(message))
    }

    pub fn mk_parse_error(&self, message: impl Into<String>) -> ParseError {
        ParseError {
            message: message.into(),
            //actual: self.peek_token().clone(),
        }
    }
}