use lexer::token::TokenType::*;
use lexer::token::{Token, TokenType};

type MoveResult = Result<usize, usize>;

///defines a way to move along a ParserCursor.
pub trait Move: Copy {
    /// Returns
    /// * `Some<usize>` - if the move succeeded, where the wrapped `usize` is the position where this move ended.
    /// * `None` - if the move did not succeed (prerequisites not satisfied)
    /// # Arguments
    /// `None` if the move did not take effect.
    ///* `at` - get token at given position
    ///* `pos` - the position in ParserCursor at beginning of the move
    fn apply<F>(&self, at: F, pos: usize) -> MoveResult
    where
        F: Fn(usize) -> Token;

    ///Used to chain `This` move with `other` move.
    /// returns a move that will first execute this move then other one only if this first succeeded.
    fn and_then<B: Move + Copy>(self, other: B) -> impl Move {
        AndThenMove {
            left: self,
            right: other,
        }
    }

    ///Used to bind `This` move with `other` move.
    /// returns a move that will first execute this move then the other one.
    fn then<B: Move + Copy>(self, other: B) -> impl Move {
        ThenMove {
            left: self,
            right: other,
        }
    }

    ///Used to execute `This` or else other if `This` fails
    /// returned move is a move that executes either this or other if this move fails.
    fn or<B: Move + Copy>(self, other: B) -> impl Move {
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
    fn apply<F>(&self, mut at: F, pos: usize) -> MoveResult
    where
        F: FnMut(usize) -> Token,
    {
        let t: Token = at(pos);
        (self.predicate)(t).then_some(pos + 1).ok_or(pos)
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
pub(crate) fn spaces() -> RepeatedMove<PredicateMove<impl Fn(Token) -> bool + Copy>> {
    repeat_n(1, of_type(Space))
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
pub(crate) fn aerated<M: Move>(m: M) -> impl Move {
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
    fn apply<F>(&self, at: F, pos: usize) -> MoveResult
    where
        F: Fn(usize) -> Token,
    {
        match self.underlying.apply(at, pos) {
            Ok(_) => Err(pos),
            Err(_) => Ok(pos),
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
    fn apply<F>(&self, at: F, pos: usize) -> MoveResult
    where
        F: Fn(usize) -> Token,
    {
        let mut repeats = 0;
        let mut current_pos = pos;
        while let Ok(pos) = self.underlying.apply(&at, current_pos) {
            current_pos = pos;
            repeats += 1;
            if self.max != -1 && repeats > self.max {
                return Err(pos); // we exceeded the maximum amount of repetitions.
            }

            if at(current_pos).token_type == EndOfFile {
                //we hit eof
                break;
            }
        }
        // We did not repeated enough time to satisfy this movement
        if self.min != -1 && repeats < self.min {
            return Err(pos);
        }
        Ok(current_pos)
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
    fn apply<F>(&self, at: F, pos: usize) -> MoveResult
    where
        F: Fn(usize) -> Token,
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
    fn apply<F>(&self, at: F, mut pos: usize) -> MoveResult
    where
        F: Fn(usize) -> Token,
    {
        if let Ok(new_pos) = self.left.apply(&at, pos) {
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
    fn apply<F>(&self, at: F, pos: usize) -> MoveResult
    where
        F: Fn(usize) -> Token,
    {
        self.left
            .apply(&at, pos)
            .or_else(|_| self.right.apply(at, pos))
    }
}

///A move that will succeed if underlying succeeds but the returned position is the starting pos of this move.
/// This move is useful for mutable movement procedure that wants to make a _partial_ lookahead.
#[derive(Copy, Clone)]
pub(crate) struct LookaheadMove<A: Move + Copy> {
    underlying: A,
}

impl<A: Move + Copy> Move for LookaheadMove<A> {
    fn apply<F>(&self, at: F, pos: usize) -> MoveResult
    where
        F: Fn(usize) -> Token,
    {
        self.underlying.apply(&at, pos).map(|_| pos)
    }
}

pub(crate) fn lookahead<M: Move + Copy>(m: M) -> LookaheadMove<M> {
    LookaheadMove { underlying: m }
}

//////////////////// STANDARD MOVES ////////////////////

/// Tests if the token ends an expression.
///
/// Use this move in expressions where line endings are important. If not, use [`eog`].
pub(crate) fn eox() -> PredicateMove<impl (Fn(Token) -> bool) + Copy> {
    like(TokenType::ends_expression)
}

/// Tests if the token ends a group.
///
/// Use this move in expressions where line endings does not matter. If not, use [`eox`].
pub(crate) fn eog() -> PredicateMove<impl (Fn(Token) -> bool) + Copy> {
    like(TokenType::ends_group)
}

/// Tests if the token acts as a line ending.
pub(crate) fn line_end() -> PredicateMove<impl (Fn(Token) -> bool) + Copy> {
    of_types(&[NewLine, SemiColon, EndOfFile])
}

pub(crate) fn identifier_parenthesis() -> impl Move {
    of_type(Identifier).and_then(of_types(&[SquaredLeftBracket, RoundedLeftBracket]))
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use lexer::token::{Token, TokenType};

    use crate::cursor::ParserCursor;
    use crate::moves::eox;

    #[test]
    fn eox_move() {
        let tokens = [Token::new(TokenType::SemiColon, 0..1)];
        let cursor = ParserCursor::new(tokens.to_vec());
        let result = cursor.lookahead(eox());
        assert_eq!(result, Some(tokens[0].clone()));
    }
}
