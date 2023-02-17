use lexer::token::{Token, TokenType};
use lexer::token::TokenType::Space;

use crate::cursor::ParserCursor;
use crate::parser::ParseResult;


///defines a way to move along a ParserCursor.
pub trait Move {
    ///poll: polls the next token
    ///pos: the position in ParserCursor at beginning of the move
    ///returns: Some<usize> if the move succeeds, where the wrapped usize is the new position of the cursor
    /// None if the move did not take effect.
    fn apply<'a, F>(&self, poll: F, pos: usize) -> Option<usize>
        where F: Fn() -> &'a Token<'a>;
}

struct PredicateMove {
    predicate: fn(&Token) -> bool,
}

impl<'m> Move for PredicateMove {
    fn apply<'a, P>(&self, poll: P, pos: usize) -> Option<usize>
        where P: Fn() -> &'a Token<'a> {
        let token = poll();

        if (self.predicate)(token) {
            Some(pos + 1)
        } else {
            None
        }
    }
}

pub fn predicate(predicate: fn(&Token) -> bool) -> PredicateMove {
    PredicateMove { predicate }
}

pub fn next() -> PredicateMove {
    predicate(|_| true)
}

pub fn no_space() -> PredicateMove {
    predicate(|t| t.token_type != Space)
}

#[inline]
pub fn of_type(set: &[TokenType]) -> PredicateMove {
    predicate(|t| set.contains(&t.token_type))
}


struct WhileMove<M: Move> {
    body: M,
}

impl<M: Move> Move for WhileMove<M> {
    fn apply<'a, F>(&self, poll: F, pos: usize) -> Option<usize> where F: Fn() -> &'a Token<'a> {
        let mut current = None;
        let mut current_pos = pos;
        while let Some(pos) = self.body.apply(&poll, current_pos) {
            current_pos = pos;
        }
        current
    }
}

pub fn aslas<'a, F, M: Move>(mov: M) -> WhileMove<M> {
    WhileMove { body: mov }
}