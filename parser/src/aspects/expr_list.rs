use crate::err::ParseErrorKind::{Expected, Unexpected};
use crate::moves::{eod, lookahead, of_type, MoveOperations, blanks};
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType;
use lexer::token::TokenType::Comma;

///An aspect to parse expression lists
pub(super) trait ExpressionListAspect<'a> {
    ///Implicit lists are whether A, (A), (A, B, ...) or () (if it can be empty)
    /// according that `(` and `)` are the [`start`]/[`end`] of the list expression.
    fn parse_implicit_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        non_empty: bool,
        parse_element: F,
    ) -> ParseResult<Vec<E>>
        where
            F: FnMut(&mut Self) -> ParseResult<E>;

    ///Explicits lists are whether (A), (A, B, ...) or () (if it can be empty)
    /// according that `(` and `)` are the start/end of the list expression.
    /// An explicit list cannot consider an expression A as valid, the expression must be surrounded
    /// with [`start`] and [`end`]
    fn parse_explicit_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        non_empty: bool,
        parse_element: F,
    ) -> ParseResult<Vec<E>>
        where
            F: FnMut(&mut Self) -> ParseResult<E>;
}

impl<'a> ExpressionListAspect<'a> for Parser<'a> {
    fn parse_implicit_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        non_empty: bool,
        mut parse_element: F,
    ) -> ParseResult<Vec<E>>
        where
            F: FnMut(&mut Self) -> ParseResult<E>,
    {
        if self.cursor.lookahead(of_type(start)).is_some() {
            self.parse_explicit_list(start, end, non_empty, parse_element)
        } else {
            Ok(vec![parse_element(self)?])
        }
    }

    fn parse_explicit_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        non_empty: bool,
        mut parse_element: F,
    ) -> ParseResult<Vec<E>>
        where
            F: FnMut(&mut Self) -> ParseResult<E>,
    {
        let start = self.cursor.force_with(
            of_type(start),
            "expected start of list expression",
            Expected(start.str().unwrap_or("<undefined>").to_string())
        )?;
        self.delimiter_stack.push_back(start.clone());
        let mut elements = vec![];

        while self.cursor.lookahead(blanks().then(eod())).is_none() {
            elements.push(match parse_element(self) {
                Err(err) => {
                    self.delimiter_stack.pop_back();
                    return Err(err)
                }
                Ok(val) => val
            });
            self.cursor.force_with(
                blanks().then(of_type(Comma).or(lookahead(eod()))),
                "A comma or a closing bracket was expected here",
                Expected(format!("',' or '{}'", end.str().unwrap_or("<undefined>")).to_string()),
            )?;
        }
        self.cursor.advance(blanks());

        if elements.is_empty() && non_empty {
            self.expect_delimiter(end)?;
            return self.expected_with(
                "unexpected empty type parameter list",
                start..self.cursor.peek(),
                Unexpected,
            );
        }

        self.cursor.advance(blanks());
        self.expect_delimiter(end)?;

        Ok(elements)
    }
}
