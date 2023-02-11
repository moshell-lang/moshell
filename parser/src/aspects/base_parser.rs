use lexer::token::{Token, TokenType};
use lexer::token::TokenType::EndOfFile;

use crate::parser::{ParseError, Parser};

///Defines all basic operations a parser can do.
pub trait BaseParser<'a> {
    fn meet_token(&mut self, expected: TokenType) -> bool;
    fn match_token(&mut self, expected: TokenType) -> Option<Token<'a>>;
    fn expect_token(&mut self, expected: TokenType, message: &str) -> Result<Token<'a>, ParseError>;
    fn peek_token(&self) -> Token<'a>;
    fn next_token(&mut self) -> Result<Token<'a>, ParseError>;
    fn is_at_end(&self) -> bool;
    fn expected(&self, message: &str) -> Result<Token<'a>, ParseError>;
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

    fn expect_token(&mut self, expected: TokenType, message: &str) -> Result<Token<'a>, ParseError> {
        self.match_token(expected).ok_or_else(|| mk_parse_error(message))
    }

    fn peek_token(&self) -> Token<'a> {
        self.tokens.get(self.current).cloned().unwrap_or(Token::new(EndOfFile, ""))
    }

    fn next_token(&mut self) -> Result<Token<'a>, ParseError> {
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

    fn expected(&self, message: &str) -> Result<Token<'a>, ParseError> {
        Err(mk_parse_error(message))
    }
}

fn mk_parse_error(message: &str) -> ParseError {
    ParseError {
        message: message.to_string(),
        //actual: self.peek_token().clone(),
    }
}