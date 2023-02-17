use lexer::token::{Token, TokenType};
use lexer::token::TokenType::Space;

use crate::cursor::ParserCursor;
use crate::parser::ParseResult;

///defines a way to move along a ParserCursor.
pub trait Move {
    /// Returns `Some<usize>` if the move succeeds, where the wrapped usize is the new position of the cursor
    /// `None` if the move did not take effect.
    ///* `poll` - polls the next token
    ///* `pos` - the position in ParserCursor at beginning of the move
    fn apply<'a, F>(&self, poll: F, pos: usize) -> Option<usize>
        where F: Fn() -> &'a Token<'a>;
}

///Defines operations over a Move struct.
pub trait MoveOperations<'a, This: Move> {
    ///Used to chain `This` move with `other` move.
    /// returns a move that will first execute this move then other one only if this first succeeded.
    fn and_then<B: Move>(&self, other: &B) -> AndThenMove<'a, This, B>;
}

impl<'a, A: Move> MoveOperations<'a, A> for A {
    fn and_then<B: Move>(&self, other: &B) -> AndThenMove<'a, Self, B> {
        AndThenMove { origin: self, other: other }
    }
}

///A Move that only move over one token and only if it satisfies its predicate.
struct PredicateMove<P>
    where P: Fn(&Token) -> bool {
    ///The used predicate
    predicate: P,
}

impl<'m, P> Move for PredicateMove<P>
    where P: Fn(&Token) -> bool {
    fn apply<'a, F>(&self, poll: F, pos: usize) -> Option<usize>
        where F: Fn() -> &'a Token<'a> {
        let token = poll();

        if (self.predicate)(token) {
            Some(pos + 1)
        } else {
            None
        }
    }
}

///construct a PredicateMove.
/// Will move once only if the given predicate is satisfied.
/// * `predicate` - the predicate to satisfy
pub fn predicate<P>(predicate: P) -> PredicateMove<P>
    where P: Fn(&Token) -> bool {
    PredicateMove { predicate }
}

///Move to next token
pub fn next() -> PredicateMove<fn(&Token) -> bool> {
    predicate(|_| true)
}

///Move to next token if it's not a space
pub fn no_space() -> PredicateMove<fn(&Token) -> bool> {
    predicate(|t| t.token_type != Space)
}

///repeats until it finds a token that's not a space
pub fn next_no_space() -> impl Move {
    repeat(predicate(|t| t.token_type == Space))
}

///Move to next token if its type is in the given set
/// * `set` - the set of TokenType to satisfy
pub fn of_type(set: &[TokenType]) -> PredicateMove<impl Fn(&Token) -> bool + '_> {
    predicate(|token: &Token| set.contains(&token.token_type))
}

/// A RepeatedMove is a special kind of move that will repeat as long as the underlying move succeeds.
struct RepeatedMove<M: Move> {
    underlying: M,
}

impl<M: Move> Move for RepeatedMove<M> {
    fn apply<'a, F>(&self, poll: F, pos: usize) -> Option<usize> where F: Fn() -> &'a Token<'a> {
        let mut current_pos = pos;
        while let Some(pos) = self.underlying.apply(&poll, current_pos) {
            current_pos = pos;
        }
        Some(current_pos)
    }
}

///Repeat the given move until it fails, exiting on the first token that made the underlying move fail.
pub fn repeat<'a, M: Move>(mov: M) -> RepeatedMove<M> {
    RepeatedMove { underlying: mov }
}


///Execute origin and then, if it succeeds, execute the other
struct AndThenMove<'a, A: Move, B: Move> {
    origin: &'a A,
    other: &'a B,
}

impl<'a, A: Move, B: Move> Move for AndThenMove<'a, A, B> {
    fn apply<'b, F>(&self, poll: F, pos: usize) -> Option<usize> where F: Fn() -> &'b Token<'b> {
        self.origin.apply(poll, pos).and_then(|pos| self.other.apply(&poll, pos))
    }
}