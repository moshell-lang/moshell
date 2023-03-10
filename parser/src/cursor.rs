use std::fmt::Debug;
use context::poller::Poller;
use crate::err::{ParseErrorKind};
use crate::moves::Move;
use lexer::token::{Token, TokenType};

use crate::parser::ParseResult;

/// Parser cursor is used by parsers to navigate in the token stream
#[derive(Debug, Clone)]
pub(crate) struct ParserCursor<'a, P: Debug> {
    /// The token poller
    poller: P,
    /// token buffer
    buff: Vec<Token<'a>>,
    /// current position in the tokens vector.
    pos: usize,
}

impl<'a, P: Poller<'a, Token<'a>> + Debug> ParserCursor<'a, P> {
    pub fn new(poller: P) -> Self {
        Self {
            poller,
            buff: Vec::new(),
            pos: 0,
        }
    }

    ///advance if next token satisfy the given move
    /// Returns the token last token covered (=validated) by the move operation.
    ///         The returned value is Some(Token) if the move succeeded, None instead.
    /// This method will move the current cursor position on where the move ended.
    pub fn advance(&mut self, mov: impl Move) -> Option<Token<'a>> {
        let result = mov.apply(|pos| self.at(pos), self.pos);

        if let Some(next_pos) = result {
            //we subtract 1 to next_pos because the move returns the next position
            //of the token, thus, we want to return the last token that this move covered.
            let token_return = next_pos.saturating_sub(1).max(self.pos);
            self.pos = next_pos;

            return Some(self.at(token_return));
        }
        None
    }

    /// Returns the token where the move ended if the move succeeds, or None instead.
    /// This method is similar to `advance` except that it does not makes the cursor change its current pos.
    pub fn lookahead(&mut self, mov: impl Move) -> Option<Token<'a>> {
        let result = mov.apply(|pos| self.at(pos), self.pos);

        if let Some(next_pos) = result {
            //we subtract 1 to next_pos because the move returns the next position
            //of the token, thus, we want to return the last token that this move covered.
            let token_return = next_pos.saturating_sub(1).max(self.pos);
            return Some(self.at(token_return));
        }
        None
    }

    /// Force the given move to succeed, or else fail with given error message.
    /// This method will move the current cursor position on where the move ended.
    pub fn force(&mut self, mov: impl Move, err: &str) -> ParseResult<Token<'a>> {
        self.advance(mov).ok_or_else(|| {
            self.mk_parse_error(err, self.at(self.pos + 1), ParseErrorKind::Unexpected)
        })
    }

    pub fn force_with(
        &mut self,
        mov: impl Move,
        err: &str,
        kind: ParseErrorKind,
    ) -> ParseResult<Token<'a>> {
        self.advance(mov)
            .ok_or_else(|| self.mk_parse_error(err, self.at(self.pos + 1), kind))
    }

    ///Advance and returns a selection of token.
    /// The returned vector is a selection between current position and the position of where this move ended.
    pub fn select(&mut self, mov: impl Move) -> Vec<Token<'a>> {
        let from = self.pos;
        if self.advance(mov).is_some() {
            let slice = &self.buff.as_slice()[from..self.pos];
            return Vec::from(slice);
        }
        Vec::new()
    }

    ///returns the token at current position
    pub fn peek(&mut self) -> Token<'a> {
        self.at(self.pos)
    }

    ///returns current token then advance or ParseError if this cursor hits the
    /// end of the stream.
    pub fn next(&mut self) -> ParseResult<Token<'a>> {
        let token = self.peek();
        self.pos += 1;
        Ok(token)
    }

    ///returns current token then advance or None if this cursor hits the
    /// end of the stream.
    pub fn next_opt(&mut self) -> Option<Token<'a>> {
        self.buff.get(self.pos).map(|t| {
            self.pos += 1;
            t.clone()
        })
    }

    ///returns token at specified position.
    fn at(&mut self, pos: usize) -> Token<'a> {
        if let Some(tk) = self.buff.get(pos) {
            return tk.clone()
        }

        let mut diff = self.buff.len() - pos;
        while diff > 0 && !self.poller.empty() {
            let next_token = self.poller.next().expect("fatal error while polling next line").unwrap();
            self.buff.push(next_token);
            diff -= 1;
        }
        
        let result = self.buff.get(pos).unwrap_or_else(|| &Token::new(TokenType::EndOfFile, ""));
        result.clone()
    }

    fn reset(&mut self) {
        self.pos = 0;
    }

    ///return true if this cursor is at the end of the
    pub fn is_at_end(&self) -> bool {
        self.poller.empty()
    }




}
