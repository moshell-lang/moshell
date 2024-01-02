use context::source::{SourceSegment, SourceSegmentHolder};
use lexer::token::TokenType;
use lexer::token::TokenType::Comma;

use crate::err::ParseErrorKind;
use crate::err::ParseErrorKind::Expected;
use crate::moves::{blanks, eog, lookahead, of_type, MoveOperations};
use crate::parser::{ParseResult, Parser};

///An aspect to parse expression lists
pub(crate) trait ExpressionListAspect<'a> {
    ///Implicit lists are whether A, (A), (A, B, ...) or () (if it can be empty)
    /// according that `(` and `)` are the [`start`]/[`end`] of the list expression.
    /// [`if_it_absent_msg`] used when an element is missing
    fn parse_implicit_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        if_it_absent_msg: &str,
        parse_element: F,
    ) -> ParseResult<(Vec<E>, SourceSegment)>
    where
        E: SourceSegmentHolder,
        F: FnMut(&mut Self) -> ParseResult<E>;

    ///Explicits lists are whether (A), (A, B, ...) or () (if it can be empty)
    /// according that `(` and `)` are the start/end of the list expression.
    /// An explicit list cannot consider an expression A as valid, the expression must be surrounded
    /// with [`start`] and [`end`]
    /// [`if_absent_msg`] is used if the list's start token is not present
    /// [`if_it_absent_msg`] used when an element is missing
    fn parse_explicit_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        if_absent_msg: &str,
        if_it_absent_msg: &str,
        parse_element: F,
    ) -> ParseResult<(Vec<E>, SourceSegment)>
    where
        E: SourceSegmentHolder,
        F: FnMut(&mut Self) -> ParseResult<E>;

    /// parses a list which is either nonexistent or explicit but nonempty.
    /// - if the current's token does not match `start`, an empty vec is returned.
    /// - else, an explicit list is parsed, that must end with `end` token.
    /// [`if_it_absent_msg`] used when an element is missing
    fn parse_optional_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        if_it_absent_msg: &str,
        parse_element: F,
    ) -> ParseResult<(Vec<E>, Option<SourceSegment>)>
    where
        E: SourceSegmentHolder,
        F: FnMut(&mut Self) -> ParseResult<E>;
}

impl<'a> ExpressionListAspect<'a> for Parser<'a> {
    fn parse_implicit_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        if_it_absent_msg: &str,
        mut parse_element: F,
    ) -> ParseResult<(Vec<E>, SourceSegment)>
    where
        E: SourceSegmentHolder,
        F: FnMut(&mut Self) -> ParseResult<E>,
    {
        if self.cursor.lookahead(of_type(start)).is_some() {
            self.parse_explicit_list(start, end, "", if_it_absent_msg, parse_element)
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
        if_absent_msg: &str,
        if_it_absent_msg: &str,
        mut parse_element: F,
    ) -> ParseResult<(Vec<E>, SourceSegment)>
    where
        E: SourceSegmentHolder,
        F: FnMut(&mut Self) -> ParseResult<E>,
    {
        self.cursor.advance(blanks());
        let start = self.cursor.force_with(
            of_type(start),
            if_absent_msg,
            Expected(start.str().unwrap_or("<undefined>").to_string()),
        )?;
        let mut elements = vec![];

        while self.cursor.lookahead(blanks().then(eog())).is_none() {
            self.cursor.advance(blanks());
            while let Some(comma) = self.cursor.advance(of_type(Comma)) {
                self.cursor.advance(blanks());
                self.report_error(self.mk_parse_error(
                    if_it_absent_msg,
                    comma.span,
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
            if let Err(err) = self.cursor.force_with(
                blanks().then(of_type(Comma).or(lookahead(eog()))),
                "A comma or a closing bracket was expected here",
                Expected(format!("',' or '{}'", end.str().unwrap_or("<undefined>"))),
            ) {
                self.report_error(err)
            }
        }
        self.cursor.advance(blanks());

        let end = self.expect_delimiter(start.clone(), end)?;

        Ok((elements, start.span.start..end.span.end))
    }

    fn parse_optional_list<E, F>(
        &mut self,
        start: TokenType,
        end: TokenType,
        if_it_absent_msg: &str,
        parse_element: F,
    ) -> ParseResult<(Vec<E>, Option<SourceSegment>)>
    where
        E: SourceSegmentHolder,
        F: FnMut(&mut Self) -> ParseResult<E>,
    {
        if self.cursor.lookahead(of_type(start)).is_none() {
            return Ok((Vec::new(), None));
        }
        self.cursor.advance(blanks());
        let (tparams, segment) =
            { self.parse_explicit_list(start, end, "", if_it_absent_msg, parse_element)? };

        Ok((tparams, Some(segment)))
    }
}
