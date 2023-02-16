use lexer::token::{Token, TokenType};
use lexer::token::TokenType::EndOfFile;
use crate::parser::{ParseError, ParseResult};

/// Parser cursor is used by parsers to navigate in the token stream
pub(crate) struct ParserCursor<'a> {
    /// The manipulated tokens
    tokens: Vec<Token<'a>>,
    /// current position in the tokens vector.
    pos: usize,
}

impl<'a> ParserCursor<'a> {
    ///Creates a new cursor at position 0 in the given token vector
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, pos: 0 }
    }

    ///advance if next token satisfy the given predicate
    pub fn advance_if<F>(&mut self, p: F) -> Option<&Token<'a>>
        where F: Fn(&Token) -> bool {
        let token = &self.peek_token();
        if p(token) {
            self.pos += 1;
            return Some(token);
        }
        None
    }

    ///advance while next tokens satisfies the given predicate
    /// the cursor will point at the first token that did not satisfied the predicate.
    pub fn advance_while<F>(&mut self, p: F)
        where F: Fn(&Token) -> bool {
        while self.advance_if(&p) != None {}
    }

    ///expect next token to satisfy given predicate, or else return ParseError with given message.
    pub fn expect_token<F>(&mut self, p: F, message: &str) -> ParseResult<&Token<'a>>
        where F: Fn(&Token) -> bool
    {
        self.advance_if(&p)
            .ok_or_else(|| self.mk_parse_error(message))
    }

    ///peeks next that is not a .
    /// can return EndOfFile token if the cursor is at the end of the token stream.
    pub fn peek_token(&self) -> &Token<'a> {
        &self.tokens
            .get(self.pos)
            .cloned()
            .unwrap_or(Token::new(EndOfFile, ""))
    }

    ///will ignore the next token if it satisfies the given predicate.
    pub fn ignore_next<F>(&mut self, p: F) -> &Token<'a>
        where F: Fn(&Token) -> bool
    {
        self.advance_if(&p);
        self.peek_token()
    }

    ///will ignore all tokens while they satisfies the given predicate,
    /// returning the first token that did not validate predicate.
    pub fn ignore_while<F>(&mut self, p: F) -> &Token<'a>
        where F: Fn(&Token) -> bool
    {
        self.advance_while(&p);
        self.peek_token()
    }

    ///advance and returns the next token that is not a space, or ParseError if this cursor hits the
    /// end of the stream.
    pub fn next_token(&mut self) -> ParseResult<&Token<'a>> {
        let token = self.peek_token();
        if token.token_type != EndOfFile {
            self.pos += 1;
            Ok(&token)
        } else {
            self.expected("Unexpected end of file.")
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.peek_token().token_type == EndOfFile
    }

    pub fn expected(&self, message: &str) -> ParseResult<&Token<'a>> {
        Err(self.mk_parse_error(message))
    }

    fn mk_parse_error(&self, message: impl Into<String>) -> ParseError {
        ParseError {
            message: message.into(),
            //actual: self.peek_token().clone(),
        }
    }
}