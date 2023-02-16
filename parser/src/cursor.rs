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


pub trait Move {
    fn apply<'a, F>(&self, poll: F) -> ParseResult<()>
        where F: Fn() -> &'a Token<'a>;
}


impl<'a> ParserCursor<'a> {
    ///Creates a new cursor at position 0 in the given token vector
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, pos: 0 }
    }

    ///advance if next token satisfy the given predicate
    pub fn advance<F>(&mut self, mov: impl Move) -> ParseResult<&Token<'a>>
        where F: Fn(&Token) -> bool {
        mov.apply(|| self.next_token())?;
        Ok(self.peek_token())
    }

    pub fn peek<F>(&self, mov: impl Move) -> ParseResult<&Token<'a>>
        where F: Fn(&Token) -> bool {
        let mut pos = self.pos;
    }

    fn at()

    ///advance and returns the next token or ParseError if this cursor hits the
    /// end of the stream.
    fn next(&mut self) -> &Token<'a> {
        let token = self.peek_next();
        self.pos += 1;
        token
    }

    ///peeks next that is not a .
    /// can return EndOfFile token if the cursor is at the end of the token stream.
    fn peek_next(&self) -> &Token<'a> {
        &self.tokens
            .get(self.pos)
            .cloned()
            .unwrap_or(Token::new(EndOfFile, ""))
    }


    pub fn is_at_end(&self) -> bool {
        self.peek_token().token_type == EndOfFile
    }


}