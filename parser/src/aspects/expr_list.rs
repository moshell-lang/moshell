use context::source::{SourceSegment, SourceSegmentHolder};
use lexer::token::TokenType;
use lexer::token::TokenType::Comma;

use crate::err::ParseErrorKind;
use crate::err::ParseErrorKind::{Expected, Unexpected};
use crate::moves::{blanks, eog, lookahead, of_type, MoveOperations};
use crate::parser::{ParseResult, Parser};

///An aspect to parse expression lists
pub(super) trait ExpressionListAspect<'a> {
    ///Implicit lists are whether A, (A), (A, B, ...) or () (if it can be empty)
    /// according that `(` and `)` are the [`start`]/[`end`] of the list expression.
    fn parse_implicit_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        parse_element: F,
    ) -> ParseResult<(Vec<E>, SourceSegment)>
    where
        E: SourceSegmentHolder,
        F: FnMut(&mut Self) -> ParseResult<E>;

    ///Explicits lists are whether (A), (A, B, ...) or () (if it can be empty)
    /// according that `(` and `)` are the start/end of the list expression.
    /// An explicit list cannot consider an expression A as valid, the expression must be surrounded
    /// with [`start`] and [`end`]
    fn parse_explicit_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        parse_element: F,
    ) -> ParseResult<(Vec<E>, SourceSegment)>
    where
        E: SourceSegmentHolder,
        F: FnMut(&mut Self) -> ParseResult<E>;

    /// parses a list which is either nonexistent or explicit but nonempty.
    /// - if the current's token does not match `start`, an empty vec is returned.
    /// - else, an explicit list is parsed, that must end with `end` token.
    ///   if the explicit list is empty, an error is returned with given error message.
    fn parse_optional_or_nonempty_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        empty_err: &str,
        parse_element: F,
    ) -> ParseResult<(Vec<E>, SourceSegment)>
    where
        E: SourceSegmentHolder,
        F: FnMut(&mut Self) -> ParseResult<E>;
}

impl<'a> ExpressionListAspect<'a> for Parser<'a> {
    fn parse_implicit_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        mut parse_element: F,
    ) -> ParseResult<(Vec<E>, SourceSegment)>
    where
        E: SourceSegmentHolder,
        F: FnMut(&mut Self) -> ParseResult<E>,
    {
        if self.cursor.lookahead(of_type(start)).is_some() {
            self.parse_explicit_list(start, end, parse_element)
        } else {
            let elem = parse_element(self)?;
            let segment = elem.segment();
            Ok((vec![elem], segment))
        }
    }

    fn parse_explicit_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        mut parse_element: F,
    ) -> ParseResult<(Vec<E>, SourceSegment)>
    where
        E: SourceSegmentHolder,
        F: FnMut(&mut Self) -> ParseResult<E>,
    {
        self.cursor.advance(blanks());
        let start = self.cursor.force_with(
            of_type(start),
            "expected start of list expression",
            Expected(start.str().unwrap_or("<undefined>").to_string()),
        )?;
        let mut elements = vec![];

        while self.cursor.lookahead(blanks().then(eog())).is_none() {
            self.cursor.advance(blanks());
            while let Some(comma) = self.cursor.advance(of_type(Comma)) {
                self.cursor.advance(blanks());
                self.report_error(self.mk_parse_error(
                    "Expected expression.",
                    comma,
                    ParseErrorKind::Unexpected,
                ));
            }

            if self.cursor.lookahead(blanks().then(eog())).is_some() {
                break;
            }

            match parse_element(self) {
                Err(err) => {
                    self.recover_from(err, of_type(Comma));
                }
                Ok(val) => {
                    elements.push(val);
                }
            };
            self.cursor.force_with(
                blanks().then(of_type(Comma).or(lookahead(eog()))),
                "A comma or a closing bracket was expected here",
                Expected(format!("',' or '{}'", end.str().unwrap_or("<undefined>"))),
            )?;
        }
        self.cursor.advance(blanks());

        let end = self.expect_delimiter(start.clone(), end)?;

        Ok((elements, self.cursor.relative_pos_ctx(start..end)))
    }

    fn parse_optional_or_nonempty_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        empty_err: &str,
        parse_element: F,
    ) -> ParseResult<(Vec<E>, SourceSegment)>
    where
        E: SourceSegmentHolder,
        F: FnMut(&mut Self) -> ParseResult<E>,
    {
        if self.cursor.lookahead(of_type(start)).is_none() {
            return Ok((Vec::new(), self.cursor.relative_pos_ctx(self.cursor.peek())));
        }
        self.cursor.advance(blanks());
        let start_token = self.cursor.peek();
        let ((tparams, segment), no_nested_errors) =
            self.observe_error_reports(|p| p.parse_explicit_list(start, end, parse_element))?;
        if no_nested_errors && tparams.is_empty() {
            return self.expected_with(empty_err, start_token..self.cursor.peek(), Unexpected);
        }
        Ok((tparams, segment))
    }
}
