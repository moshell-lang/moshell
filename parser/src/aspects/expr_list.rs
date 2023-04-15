use crate::err::ParseErrorKind;
use crate::err::ParseErrorKind::Expected;
use crate::moves::{blanks, eod, lookahead, of_type, MoveOperations};
use crate::parser::{ParseResult, Parser};
use context::source::{SourceSegment, SourceSegmentHolder};
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
        self.delimiter_stack.push_back(start.clone());
        let mut elements = vec![];

        while self.cursor.lookahead(blanks().then(eod())).is_none() {
            while let Some(comma) = self.cursor.advance(of_type(Comma)) {
                self.report_error(self.mk_parse_error(
                    "Expected value.",
                    comma,
                    ParseErrorKind::Unexpected,
                ));
            }
            match parse_element(self) {
                Err(err) => {
                    self.recover_from(err);
                }
                Ok(val) => {
                    elements.push(val);
                }
            };
            self.cursor.force_with(
                blanks().then(of_type(Comma).or(lookahead(eod()))),
                "A comma or a closing bracket was expected here",
                Expected(format!("',' or '{}'", end.str().unwrap_or("<undefined>")).to_string()),
            )?;
        }
        self.cursor.advance(blanks());

        let end = self.expect_delimiter(end)?;

        Ok((elements, self.cursor.relative_pos_ctx(start..end)))
    }
}
