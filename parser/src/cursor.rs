use crate::err::{ErrorContext, ParseError, ParseErrorKind};
use crate::moves::Move;
use context::source::Location;
use lexer::token::{Token, TokenType};

use crate::parser::ParseResult;

/// Parser cursor is used by parsers to navigate in the token stream
#[derive(Debug, Clone)]
pub(crate) struct ParserCursor<'a> {
    /// The manipulated tokens
    tokens: Vec<Token<'a>>,
    /// current position in the tokens vector.
    pos: usize,
    /// The source code of the tokens.
    ///
    /// This must contains all the string slices in the tokens present in the `tokens` vector.
    source: &'a str,
}

impl<'a> ParserCursor<'a> {
    ///Creates a new cursor at position 0 in the given token vector
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            tokens,
            pos: 0,
            source: "",
        }
    }

    pub fn new_with_source(tokens: Vec<Token<'a>>, source: &'a str) -> Self {
        Self {
            tokens,
            pos: 0,
            source,
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
    pub fn lookahead(&self, mov: impl Move) -> Option<Token<'a>> {
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
    pub fn collect(&mut self, mov: impl Move) -> Vec<Token<'a>> {
        let from = self.pos;
        if self.advance(mov).is_some() {
            let slice = &self.tokens.as_slice()[from..self.pos];
            return Vec::from(slice);
        }
        Vec::new()
    }

    ///returns the token at current position
    pub fn peek(&self) -> Token<'a> {
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
        self.tokens.get(self.pos).map(|t| {
            self.pos += 1;
            t.clone()
        })
    }

    ///returns token at specified position.
    fn at(&self, pos: usize) -> Token<'a> {
        self.tokens.get(pos).cloned().unwrap_or_else(|| {
            // Return a pointer to the end of the source code if there is no more token.
            Token::new(TokenType::EndOfFile, &self.source[self.source.len()..])
        })
    }

    ///return true if this cursor is at the end of the
    pub fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Create a new parse error with the given message and context.
    ///
    /// The [`expected`](Parser#expected_with) method wraps the error in a [`Err`].
    pub fn mk_parse_error(
        &self,
        message: impl Into<String>,
        context: impl Into<ErrorContext<'a>>,
        kind: ParseErrorKind,
    ) -> ParseError {
        ParseError {
            message: message.into(),
            position: self.relative_pos_ctx(context),
            kind,
        }
    }

    /// Get the relative byte offset of the given string in the source code.
    ///
    /// # Panics
    /// This method panics if the given string is not contained in the source code.
    pub fn relative_pos<'s>(&self, str: impl Into<&'s str>) -> Location {
        let str = str.into();
        let start = (str.as_ptr() as usize)
            .checked_sub(self.source.as_ptr() as usize)
            .expect("String is not contained in the source code.");
        let end = start + str.len();
        start..end
    }

    /// Get the relative byte offset of the given context in the source code.
    ///
    /// # Panics
    /// This method panics if the given context is not contained in the source code.
    pub fn relative_pos_ctx(&self, context: impl Into<ErrorContext<'a>>) -> Location {
        let context = context.into();
        let start = (context.from.as_ptr() as usize)
            .checked_sub(self.source.as_ptr() as usize)
            .expect("Context start is not contained in the source code.");
        let end = context.to.as_ptr() as usize + context.to.len() as usize
            - self.source.as_ptr() as usize;
        start..end
    }
}
