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

mod Moves {
    use crate::cursor::Move;

    pub fn next() -> impl Move {

    }
}
impl<'a> ParserCursor<'a> {
    ///Creates a new cursor at position 0 in the given token vector
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, pos: 0 }
    }

    ///advance if next token satisfy the given predicate
    pub fn advance<F>(&mut self, mov: impl Move) -> ParseResult<&Token<'a>>
        where F: Fn(&Token) -> bool {
        mov.apply(|| self.next())?;
        Ok(self.peek_next())
    }

    pub fn peek<F>(&self, mov: impl Move) -> ParseResult<&Token<'a>>
        where F: Fn(&Token) -> bool {
        let mut pos = self.pos;
        mov.apply(|| {
            let result = self.at(pos);
            pos += 1;
            result
        });
        Ok(self.at(pos))
    }

    fn at(&self, pos: usize) -> &Token<'a> {
        &self.tokens
            .get(pos)
            .cloned()
            .unwrap_or(Token::new(EndOfFile, ""))
    }

    ///advance and returns the next token or ParseError if this cursor hits the
    /// end of the stream.
    fn next(&mut self) -> &Token<'a> {
        let token = self.at(self.pos);
        self.pos += 1;
        token
    }

    ///peeks next that is not a .
    /// can return EndOfFile token if the cursor is at the end of the token stream.
    fn peek_next(&self) -> &Token<'a> {
        self.at(self.pos)
    }


    pub fn is_at_end(&self) -> bool {
        self.peek_next().token_type == EndOfFile
    }


}