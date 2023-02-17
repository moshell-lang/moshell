use lexer::token::Token;
use lexer::token::TokenType::EndOfFile;
use crate::moves::Move;

use crate::parser::{ParseError, ParseResult};

/// Parser cursor is used by parsers to navigate in the token stream
#[derive(Debug, Clone)]
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
    pub fn advance(&mut self, mov: impl Move) -> Option<Token<'a>> {
        let result = mov.apply(|pos| self.at(pos), self.pos);

        if let Some(new_pos) = result {
            self.pos = new_pos;
            return Some(self.at(new_pos));
        }
        None
    }

    pub fn lookahead(&self, mov: impl Move) -> Option<Token<'a>> {
        let result = mov.apply(|pos| self.at(pos), self.pos);

        if let Some(new_pos) = result {
            return Some(self.at(new_pos));
        }
        None
    }

    pub fn force(&mut self, mov: impl Move, err: &str) -> ParseResult<Token<'a>> {
        self.advance(mov).ok_or_else(|| ParseError {
            message: err.to_string(),
        })
    }

    pub fn peek(&self) -> Token<'a> {
        self.at(self.pos)
    }

    ///advance and returns the next token or ParseError if this cursor hits the
    /// end of the stream.
    pub fn next(&mut self) -> ParseResult<Token<'a>> {
        let nxt = self.tokens
            .get(self.pos)
            .map(|t| t.clone())
            .ok_or(ParseError {
                message: "Unexpected end of file".to_string(),
            });
        self.pos += 1;
        nxt
    }

    fn at(&self, pos: usize) -> Token<'a> {
        self.tokens
            .get(pos)
            .cloned()
            .unwrap_or(Token::new(EndOfFile, ""))
    }

    ///peeks next that is not a .
    /// can return EndOfFile token if the cursor is at the end of the token stream.
    fn peek_next(&self) -> Token<'a> {
        self.at(self.pos)
    }


    pub fn is_at_end(&self) -> bool {
        self.peek_next().token_type == EndOfFile
    }
}