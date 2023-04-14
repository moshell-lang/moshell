use lexer::token::TokenType::*;
use lexer::token::{Token, TokenType};

///defines a way to move along a ParserCursor.
pub trait Move {
    /// Returns
    /// * `Some<usize>` - if the move succeeded, where the wrapped `usize` is the position where this move ended.
    /// * `None` - if the move did not succeed (prerequisites not satisfied)
    /// # Arguments
    /// `None` if the move did not take effect.
    ///* `at` - get token at given position
    ///* `pos` - the position in ParserCursor at beginning of the move
    fn apply<'a, F>(&self, at: F, pos: usize) -> Option<usize>
    where
        F: Fn(usize) -> Token<'a>;
}

///Defines operations over a Move struct.
pub(crate) trait MoveOperations<This: Move + Copy> {
    ///Used to chain `This` move with `other` move.
    /// returns a move that will first execute this move then other one only if this first succeeded.
    fn and_then<B: Move + Copy>(self, other: B) -> AndThenMove<This, B>;

    ///Used to bind `This` move with `other` move.
    /// returns a move that will first execute this move then the other one.
    fn then<B: Move + Copy>(self, other: B) -> ThenMove<This, B>;

    ///Used to execute `This` or else other if `This` fails
    /// returned move is a move that executes either this or other if this move fails.
    fn or<B: Move + Copy>(self, other: B) -> OrMove<This, B>;
}

impl<A: Move + Copy> MoveOperations<A> for A {
    fn and_then<B: Move + Copy>(self, other: B) -> AndThenMove<Self, B> {
        AndThenMove {
            left: self,
            right: other,
        }
    }

    fn then<B: Move + Copy>(self, other: B) -> ThenMove<Self, B> {
        ThenMove {
            left: self,
            right: other,
        }
    }

    fn or<B: Move + Copy>(self, other: B) -> OrMove<Self, B> {
        OrMove {
            left: self,
            right: other,
        }
    }
}

///A Move that only move over one token and only if it satisfies its predicate.
#[derive(Copy, Clone)]
pub(crate) struct PredicateMove<P>
where
    P: Fn(Token) -> bool + Copy,
{
    ///The used predicate
    predicate: P,
}

impl<P> PredicateMove<P>
where
    P: Fn(Token) -> bool + Copy,
{
    /// Invert the current predicate.
    pub fn negate(self) -> PredicateMove<impl Fn(Token) -> bool + Copy> {
        PredicateMove {
            predicate: move |t| !(self.predicate)(t),
        }
    }
}

impl<P> Move for PredicateMove<P>
where
    P: Fn(Token) -> bool + Copy,
{
    fn apply<'a, F>(&self, mut at: F, pos: usize) -> Option<usize>
    where
        F: FnMut(usize) -> Token<'a>,
    {
        let t: Token = at(pos);
        (self.predicate)(t).then_some(pos + 1)
    }
}

///construct a PredicateMove.
/// Will move once only if the given predicate is satisfied.
/// * `predicate` - the predicate to satisfy
pub(crate) fn predicate<P>(predicate: P) -> PredicateMove<P>
where
    P: Fn(Token) -> bool + Copy,
{
    PredicateMove { predicate }
}

/// a predicate move on the type of the token rather than it's integrity
pub(crate) fn like<P>(predicate: P) -> PredicateMove<impl Fn(Token) -> bool + Copy>
where
    P: Fn(TokenType) -> bool + Copy,
{
    PredicateMove {
        predicate: move |t| predicate(t.token_type),
    }
}

///Move to next token if its type is in the given set
/// * `set` - the set of TokenType to satisfy
pub(crate) fn of_types(set: &[TokenType]) -> PredicateMove<impl Fn(Token) -> bool + '_ + Copy> {
    predicate(move |token| set.contains(&token.token_type))
}

///Move to next token if its type match the given tpe param
pub(crate) fn of_type(tpe: TokenType) -> PredicateMove<impl Fn(Token) -> bool + Copy> {
    predicate(move |token| tpe == token.token_type)
}

///Move to next token until we reach EOF
pub(crate) fn next() -> PredicateMove<fn(Token) -> bool> {
    predicate(|t| t.token_type != EndOfFile)
}

///Accept any token
pub(crate) fn any() -> PredicateMove<fn(Token) -> bool> {
    predicate(|_| true)
}

///A move that always fails
pub(crate) fn fail() -> PredicateMove<fn(Token) -> bool> {
    predicate(|_| false)
}

///A move that consumes noting
pub(crate) fn none() -> PredicateMove<fn(Token) -> bool> {
    predicate(|_| false)
}

///repeats until it finds a token that's not a space
pub(crate) fn spaces() -> PredicateMove<impl (for<'a> Fn(Token<'a>) -> bool) + Copy> {
    of_type(Space)
}

///a move to consume any space or any newline
pub(crate) fn blank() -> PredicateMove<impl Fn(Token) -> bool + Copy> {
    of_types(&[Space, NewLine])
}

///a move to consume any spaces or any newlines
pub(crate) fn blanks() -> RepeatedMove<PredicateMove<impl Fn(Token) -> bool + Copy>> {
    repeat(of_types(&[Space, NewLine]))
}

///a move to consume a move between spaces and newlines, this move succeeds only if the given move
/// succeeds.
pub(crate) fn aerated<M: Move + Copy>(
    m: M,
) -> AndThenMove<
    ThenMove<RepeatedMove<PredicateMove<impl (for<'a> Fn(Token<'a>) -> bool) + Copy>>, M>,
    RepeatedMove<PredicateMove<impl (for<'a> Fn(Token<'a>) -> bool) + Copy>>,
> {
    blanks().then(m).and_then(blanks())
}

/// A Move to inverse the matching status of underlying move.
/// If underlying succeeds: fail
/// if underlying fails: succeed at given pos.
#[derive(Copy, Clone)]
pub(crate) struct NotMove<M: Move + Copy> {
    underlying: M,
}

impl<M: Move + Copy> Move for NotMove<M> {
    fn apply<'a, F>(&self, at: F, pos: usize) -> Option<usize>
    where
        F: Fn(usize) -> Token<'a>,
    {
        match self.underlying.apply(at, pos) {
            Some(_) => None,
            None => Some(pos),
        }
    }
}

/// inverse the matching status of input move.
/// If underlying succeeds: fail
/// if underlying fails: succeed at given pos.
pub(crate) fn not<M: Move + Copy>(m: M) -> NotMove<M> {
    NotMove { underlying: m }
}

/// A RepeatedMove is a special kind of move that will repeat as long as the underlying move succeeds
/// and until it hits the end of token stream.
#[derive(Copy, Clone)]
pub(crate) struct RepeatedMove<M: Move + Copy> {
    underlying: M,
    min: isize,
    max: isize,
}

impl<M: Move + Copy> Move for RepeatedMove<M> {
    fn apply<'a, F>(&self, at: F, pos: usize) -> Option<usize>
    where
        F: Fn(usize) -> Token<'a>,
    {
        let mut repeats = 0;
        let mut current_pos = pos;
        while let Some(pos) = self.underlying.apply(&at, current_pos) {
            current_pos = pos;
            repeats += 1;
            if self.max != -1 && repeats > self.max {
                return None; // we exceeded the maximum amount of repetitions.
            }

            if at(current_pos).token_type == EndOfFile {
                //we hit eof
                break;
            }
        }
        // We did not repeated enough time to satisfy this movement
        if self.min != -1 && repeats < self.min {
            return None;
        }
        Some(current_pos)
    }
}

///Repeats the given move until it fails,
/// exiting on the first token that made the underlying move fail or if it hits EOF.
/// NOTE: a repeat always succeed
pub(crate) fn repeat<M: Move + Copy>(mov: M) -> RepeatedMove<M> {
    RepeatedMove {
        underlying: mov,
        min: -1,
        max: -1,
    }
}

///Repeat at least n times the given move until it fails,
/// exiting on the first token that made the underlying move fail or if it hits EOF.
/// if the number of repetition is strictly inferior than n, the move fails
pub(crate) fn repeat_n<M: Move + Copy>(n: usize, mov: M) -> RepeatedMove<M> {
    RepeatedMove {
        underlying: mov,
        min: n as isize,
        max: -1,
    }
}

///Repeats between n and m times the given move until it fails,
/// exiting on the first token that made the underlying move fail or if it hits EOF.
/// if the number of repetition is strictly inferior than n, the move fails
/// if the number of repetition is strictly superior than m, the move also fails
pub(crate) fn repeat_nm<M: Move + Copy>(n: usize, m: usize, mov: M) -> RepeatedMove<M> {
    RepeatedMove {
        underlying: mov,
        min: n as isize,
        max: m as isize,
    }
}

///Given move must succeed exactly n times.
pub(crate) fn times<M: Move + Copy>(n: usize, mov: M) -> RepeatedMove<M> {
    repeat_nm(n, n, mov)
}

///Execute origin and then, if it succeeds, execute the other
#[derive(Copy, Clone)]
pub(crate) struct AndThenMove<A: Move + Copy, B: Move + Copy> {
    left: A,
    right: B,
}

impl<A: Move + Copy, B: Move + Copy> Move for AndThenMove<A, B> {
    fn apply<'a, F>(&self, at: F, pos: usize) -> Option<usize>
    where
        F: Fn(usize) -> Token<'a>,
    {
        self.left
            .apply(&at, pos)
            .and_then(|pos| self.right.apply(&at, pos))
    }
}

///Execute origin and then, if it succeeds, execute the other
#[derive(Copy, Clone)]
pub(crate) struct ThenMove<A: Move + Copy, B: Move + Copy> {
    left: A,
    right: B,
}

impl<A: Move + Copy, B: Move + Copy> Move for ThenMove<A, B> {
    fn apply<'a, F>(&self, at: F, mut pos: usize) -> Option<usize>
    where
        F: Fn(usize) -> Token<'a>,
    {
        if let Some(new_pos) = self.left.apply(&at, pos) {
            pos = new_pos
        }
        self.right.apply(&at, pos)
    }
}

///Execute left or right.
#[derive(Copy, Clone)]
pub(crate) struct OrMove<A: Move + Copy, B: Move + Copy> {
    left: A,
    right: B,
}

impl<A: Move + Copy, B: Move + Copy> Move for OrMove<A, B> {
    fn apply<'a, F>(&self, at: F, pos: usize) -> Option<usize>
    where
        F: Fn(usize) -> Token<'a>,
    {
        self.left
            .apply(&at, pos)
            .or_else(|| self.right.apply(at, pos))
    }
}

///A move that will succeed if underlying succeeds but the returned position is the starting pos of this move.
/// This move is useful for mutable movement procedure that wants to make a _partial_ lookahead.
#[derive(Copy, Clone)]
pub(crate) struct LookaheadMove<A: Move + Copy> {
    underlying: A,
}

impl<A: Move + Copy> Move for LookaheadMove<A> {
    fn apply<'a, F>(&self, at: F, pos: usize) -> Option<usize>
    where
        F: Fn(usize) -> Token<'a>,
    {
        self.underlying.apply(&at, pos).map(|_| pos)
    }
}

pub(crate) fn lookahead<M: Move + Copy>(m: M) -> LookaheadMove<M> {
    LookaheadMove { underlying: m }
}

//////////////////// STANDARD MOVES ////////////////////

///End of _group_ Delimiter, any closing punctuation as long as they are unescaped
pub(crate) fn eod() -> PredicateMove<impl (for<'a> Fn(Token<'a>) -> bool) + Copy> {
    like(TokenType::is_closing_ponctuation)
}

///a move to consume default eox tokens as long as they are not escaped.
/// default eox tokens are semicolon (;) and newline (\n)
pub(crate) fn eox() -> PredicateMove<impl (for<'a> Fn(Token<'a>) -> bool) + Copy> {
    of_types(&[NewLine, SemiColon, EndOfFile])
}
///a move that consumes a character if it can be escaped.
pub(crate) fn escapable() -> PredicateMove<impl (for<'a> Fn(Token<'a>) -> bool) + Copy> {
    like(TokenType::is_ponctuation)
}

///a move that consumes a binary operation character
pub(crate) fn bin_op() -> PredicateMove<impl (for<'a> Fn(Token<'a>) -> bool) + Copy> {
    like(TokenType::is_bin_operator)
}

pub(crate) fn identifier_parenthesis() -> AndThenMove<
    PredicateMove<impl Fn(Token) -> bool + Copy + Sized>,
    PredicateMove<impl Fn(Token) -> bool + Copy + Sized>,
> {
    of_type(Identifier).and_then(of_types(&[RoundedLeftBracket, SquaredLeftBracket]))
}

#[cfg(test)]
mod tests {
    use crate::cursor::ParserCursor;
    use crate::moves::eox;
    use lexer::lexer::lex;
    use lexer::token::{Token, TokenType};
    use pretty_assertions::assert_eq;

    #[test]
    fn eox_move() {
        let tokens = lex(";");
        let cursor = ParserCursor::new(tokens);
        let result = cursor.lookahead(eox());
        assert_eq!(result, Some(Token::new(TokenType::SemiColon, ";")));
    }
}
