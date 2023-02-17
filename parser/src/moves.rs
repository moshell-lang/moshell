use lexer::token::{Token, TokenType};
use lexer::token::TokenType::Space;

///defines a way to move along a ParserCursor.
pub trait Move {
    /// Returns
    /// * `Some<usize>` - if the move succeeded, where the wrapped `usize` is the position where this move ended.
    /// * `None` - if the move did not succeed (prerequisites not satisfied)
    /// # Arguments
    /// `None` if the move did not take effect.
    ///* `poll` - polls the next token
    ///* `pos` - the position in ParserCursor at beginning of the move
    fn apply<'a, F>(&self, poll: F, pos: usize) -> Option<usize>
        where F: FnMut() -> Token<'a>;
}

///Defines operations over a Move struct.
pub trait MoveOperations<'a, This: Move> {
    ///Used to chain `This` move with `other` move.
    /// returns a move that will first execute this move then other one only if this first succeeded.
    fn and_then<B: Move>(self, other: B) -> AndThenMove<This, B>;
}

impl<'a, A: Move> MoveOperations<'a, A> for A {
    fn and_then<B: Move>(self, other: B) -> AndThenMove<Self, B> {
        AndThenMove { origin: self, other: other }
    }
}

///A Move that only move over one token and only if it satisfies its predicate.
struct PredicateMove<P>
    where P: Fn(Token) -> bool {
    ///The used predicate
    predicate: P,
}

impl<'m, P> Move for PredicateMove<P>
    where P: Fn(Token) -> bool {
    fn apply<'a, F>(&self, mut poll: F, pos: usize) -> Option<usize>
        where F: FnMut() -> Token<'a> {
        let token = poll();
        (self.predicate)(token).then(|| pos + 1)
    }
}

///construct a PredicateMove.
/// Will move once only if the given predicate is satisfied.
/// * `predicate` - the predicate to satisfy
pub fn predicate<P>(predicate: P) -> PredicateMove<P>
    where P: Fn(Token) -> bool {
    PredicateMove { predicate }
}

///Move to next token
pub fn next() -> PredicateMove<fn(Token) -> bool> {
    predicate(|_| true)
}

///Move to next token if it's not a space
pub fn no_space() -> PredicateMove<fn(Token) -> bool> {
    predicate(|t| t.token_type != Space)
}

///Move to next token if it's a space
pub fn space() -> PredicateMove<fn(Token) -> bool> {
    predicate(|t| t.token_type == Space)
}

///repeats until it finds a token that's not a space
pub fn next_no_space() -> impl Move {
    repeat(predicate(|t| t.token_type == Space))
}

///Move to next token if its type is in the given set
/// * `set` - the set of TokenType to satisfy
pub fn of_types(set: &[TokenType]) -> PredicateMove<impl Fn(Token) -> bool + '_> {
    predicate(move |token| set.contains(&token.token_type))
}

pub fn of_type(tpe: TokenType) -> PredicateMove<impl Fn(Token) -> bool> {
    predicate(move |token| tpe == token.token_type)
}


/// A RepeatedMove is a special kind of move that will repeat as long as the underlying move succeeds.
struct RepeatedMove<M: Move> {
    underlying: M,
}

impl<M: Move> Move for RepeatedMove<M> {
    fn apply<'a, F>(&self, mut poll: F, pos: usize) -> Option<usize>
        where F: FnMut() -> Token<'a> {
        let mut current_pos = pos;
        while let Some(pos) = self.underlying.apply(&mut poll, current_pos) {
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
struct AndThenMove<A: Move, B: Move> {
    origin: A,
    other: B,
}

impl<A: Move, B: Move> Move for AndThenMove<A, B> {
    fn apply<'b, F>(&self, mut poll: F, pos: usize) -> Option<usize>
        where F: FnMut() -> Token<'b> {
        self.origin.apply(&mut poll, pos).and_then(|pos| self.other.apply(poll, pos))
    }
}