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
pub(crate) trait MoveOperations<This: Move + Clone> {
    ///Used to chain `This` move with `other` move.
    /// returns a move that will first execute this move then other one only if this first succeeded.
    fn and_then<B: Move + Clone>(&self, other: B) -> AndThenMove<This, B>;

    ///Used to bind `This` move with `other` move.
    /// returns a move that will first execute this move then the other one.
    fn then<B: Move + Clone>(&self, other: B) -> ThenMove<This, B>;

    ///Used to execute `This` or else other if `This` fails
    /// returned move is a move that executes either this or other if this move fails.
    fn or<B: Move + Clone>(&self, other: B) -> OrMove<This, B>;
}


impl<A: Move + Clone + Clone> MoveOperations<A> for A {
    fn and_then<B: Move + Clone>(&self, other: B) -> AndThenMove<Self, B> {
        AndThenMove {
            left: self.clone(),
            right: other,
        }
    }
    fn then<B: Move + Clone>(&self, other: B) -> ThenMove<Self, B> {
        ThenMove {
            left: self.clone(),
            right: other,
        }
    }
    fn or<B: Move + Clone>(&self, other: B) -> OrMove<Self, B> {
        OrMove {
            left: self.clone(),
            right: other,
        }
    }
}

///A Move that only move over one token and only if it satisfies its predicate.
#[derive(Clone)]
pub(crate) struct PredicateMove<P>
    where
        P: Fn(Token) -> bool + Clone,
{
    ///The used predicate
    predicate: P,
}

impl<P> PredicateMove<P>
    where
        P: Fn(Token) -> bool + Clone {
    /// Invert the current predicate.
    pub fn negate(self) -> PredicateMove<impl Fn(Token) -> bool + Clone> {
        PredicateMove {
            predicate: move |t| !(self.predicate)(t)
        }
    }
}

impl<P> Move for PredicateMove<P>
    where
        P: Fn(Token) -> bool + Clone,
{
    fn apply<'a, F>(&self, mut at: F, pos: usize) -> Option<usize>
        where
            F: FnMut(usize) -> Token<'a>,
    {
        let t = at(pos);
        (self.predicate)(t).then_some(pos + 1)
    }
}

///construct a PredicateMove.
/// Will move once only if the given predicate is satisfied.
/// * `predicate` - the predicate to satisfy
pub(crate) fn predicate<P>(predicate: P) -> PredicateMove<P>
    where
        P: Fn(Token) -> bool + Clone,
{
    PredicateMove { predicate }
}

///Move to next token if its type is in the given set
/// * `set` - the set of TokenType to satisfy
pub(crate) fn of_types(set: &[TokenType]) -> PredicateMove<impl Fn(Token) -> bool + '_ + Clone> {
    predicate(move |token| set.contains(&token.token_type))
}

pub(crate) fn of_type(tpe: TokenType) -> PredicateMove<impl Fn(Token) -> bool + Clone> {
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

///Move to next token if it's not a space
pub(crate) fn no_space() -> PredicateMove<impl for<'a> Fn(Token<'a>) -> bool + Clone> {
    of_type(Space).negate()
}

///Move to next token if it's a space
pub(crate) fn space() -> PredicateMove<impl for<'a> Fn(Token<'a>) -> bool + Clone> {
    of_type(Space)
}

///repeats until it finds a token that's not a space
pub(crate) fn spaces() -> impl Move + Clone {
    repeat_n(1, space())
}


/// A RepeatedMove is a special kind of move that will repeat as long as the underlying move succeeds.
#[derive(Clone)]
pub(crate) struct RepeatedMove<M: Move + Clone> {
    underlying: M,
    min: isize,
    max: isize,
}

impl<M: Move + Clone> Move for RepeatedMove<M> {
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
        }
        // We do not repeated enough time to satisfy this movement
        if self.min != -1 && repeats < self.min {
            return None;
        }
        Some(current_pos)
    }
}

///Repeats the given move until it fails, exiting on the first token that made the underlying move fail.
/// NOTE: a repeat always succeed
pub(crate) fn repeat<M: Move + Clone>(mov: M) -> RepeatedMove<M> {
    RepeatedMove { underlying: mov, min: -1, max: -1 }
}

///Repeat at least n times the given move until it fails, exiting on the first token that made the underlying move fail.
/// /// if the number of repetition is strictly inferior than n, the move fails
/// NOTE: a repeat always succeed
pub(crate) fn repeat_n<M: Move + Clone>(n: usize, mov: M) -> RepeatedMove<M> {
    RepeatedMove { underlying: mov, min: n as isize, max: -1 }
}

///Repeats between n and m times the given move until it fails, exiting on the first token that made the underlying move fail.
/// if the number of repetition is strictly inferior than n, the move fails
/// if the number of repetition is strictly superior than m, the move also fails
/// NOTE: a repeat always succeed
pub(crate) fn repeat_nm<M: Move + Clone>(n: usize, m: usize, mov: M) -> RepeatedMove<M> {
    RepeatedMove { underlying: mov, min: n as isize, max: m as isize }
}

///Execute origin and then, if it succeeds, execute the other
#[derive(Clone)]
pub(crate) struct AndThenMove<A: Move + Clone, B: Move + Clone> {
    left: A,
    right: B,
}

impl<A: Move + Clone, B: Move + Clone> Move for AndThenMove<A, B> {
    fn apply<'b, F>(&self, at: F, pos: usize) -> Option<usize>
        where
            F: Fn(usize) -> Token<'b>,
    {
        self.left
            .apply(&at, pos)
            .and_then(|pos| self.right.apply(&at, pos))
    }
}

///Execute origin and then, if it succeeds, execute the other
#[derive(Clone)]
pub(crate) struct ThenMove<A: Move + Clone, B: Move + Clone> {
    left: A,
    right: B,
}

impl<A: Move + Clone, B: Move + Clone> Move for ThenMove<A, B> {
    fn apply<'b, F>(&self, at: F, mut pos: usize) -> Option<usize>
        where
            F: Fn(usize) -> Token<'b>,
    {
        if let Some(new_pos) = self.left.apply(&at, pos) {
            pos = new_pos
        }
        self.right.apply(&at, pos)
    }
}

///Execute left or right.
#[derive(Clone)]
pub(crate) struct OrMove<A: Move + Clone, B: Move + Clone> {
    left: A,
    right: B,
}

impl<A: Move + Clone, B: Move + Clone> Move for OrMove<A, B> {
    fn apply<'b, F>(&self, at: F, pos: usize) -> Option<usize>
        where
            F: Fn(usize) -> Token<'b>,
    {
        self.left.apply(&at, pos).or_else(|| self.right.apply(at, pos))
    }
}

//////////////////// STANDARD MOVES ////////////////////

//a move to consume semicolons or new lines as long as they are not escaped.
pub(crate) fn eox() -> OrMove<
    AndThenMove<
        PredicateMove<impl (for<'a> Fn(Token<'a>) -> bool) + Clone>,
        PredicateMove<impl (for<'a> Fn(Token<'a>) -> bool) + Clone + 'static>
    >,
    PredicateMove<impl (for<'a> Fn(Token<'a>) -> bool) + Clone + 'static>
>
{

    //if it's escaped then it's not an EOX
    (of_type(BackSlash).and_then(escapable().negate()))

        //else it must be either new line or ';'
        .or(of_types(&[NewLine, SemiColon]))
}

///a move that consumes a character if it can be escaped.
pub(crate) fn escapable() -> PredicateMove<impl (for<'a> Fn(Token<'a>) -> bool) + Clone + 'static> {
    of_types(&[NewLine, Pipe, And, Or, SemiColon, Ampersand])
}



#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
    use lexer::token::{Token, TokenType};
    use crate::cursor::ParserCursor;
    use crate::moves::eox;

    #[test]
    fn eox_move() {
        let tokens = lex(";");
        let cursor = ParserCursor::new(tokens);
        let result = cursor.lookahead(eox());
        assert_eq!(result, Some(Token::new(TokenType::SemiColon, ";")));
    }

    #[test]
    fn eox_move_escaped() {
        let tokens = lex("\\;");
        let cursor = ParserCursor::new(tokens);
        let result = cursor.lookahead(eox());
        assert_eq!(result, None);
    }

}
