use ast::Expr;
use context::source::SourceSegment;
use lexer::delimiter::UnmatchedDelimiter;
use lexer::token::Token;

use crate::parser::ParseResult;

/// An error that occurs during parsing.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub position: SourceSegment,
    pub kind: ParseErrorKind,
}

/// A builder to position an error that covers multiple tokens.
#[derive(Debug, PartialEq)]
pub(crate) struct ErrorContext<'a> {
    pub(crate) from: &'a str,
    pub(crate) to: &'a str,
}

impl<'a> From<Token<'a>> for ErrorContext<'a> {
    fn from(token: Token<'a>) -> Self {
        Self {
            from: token.value,
            to: token.value,
        }
    }
}

impl<'a> From<std::ops::Range<Token<'a>>> for ErrorContext<'a> {
    fn from(range: std::ops::Range<Token<'a>>) -> Self {
        Self {
            from: range.start.value,
            to: range.end.value,
        }
    }
}

impl<'a> From<&[Token<'a>]> for ErrorContext<'a> {
    fn from(tokens: &[Token<'a>]) -> Self {
        Self {
            from: tokens.first().map(|t| t.value).expect("Empty token slice."),
            to: tokens.last().map(|t| t.value).unwrap(),
        }
    }
}

impl<'a> From<&'a str> for ErrorContext<'a> {
    fn from(str: &'a str) -> Self {
        Self { from: str, to: str }
    }
}

impl<'a> From<std::ops::Range<&'a str>> for ErrorContext<'a> {
    fn from(range: std::ops::Range<&'a str>) -> Self {
        Self {
            from: range.start,
            to: range.end,
        }
    }
}

/// The kind of error that occurred.
///
/// This is used to categorize the error and to provide more information
/// when available.
#[derive(Debug, PartialEq, Clone)]
pub enum ParseErrorKind {
    /// A specific token was expected, but another token was found.
    Expected(String),

    /// A unexpected token was found in the current context.
    Unexpected,

    /// A token was found in a context where it is not allowed.
    ///
    /// This is the same as `Unexpected`, but the error also
    /// explains what could be changed to fix the error.
    UnexpectedInContext(String),

    /// A group has been opened, but not closed.
    ///
    /// This reports the location of the opening token.
    Unpaired(SourceSegment),

    /// An incorrectly formatted value.
    InvalidFormat,
}

/// The parsing result.
///
/// This contains all the parsed expressions and the errors that occurred
/// during parsing. The parser will try to recover from errors and continue
/// parsing, but always assume that if there is any error, the whole parsing
/// process have failed.
#[derive(Debug, PartialEq)]
pub struct ParseReport<'a> {
    /// Lists all parsed expressions.
    ///
    /// This may be empty if the input is empty or if all read expressions
    /// were invalid. Note that a parse error may have occurred between two
    /// valid expressions, so always check for errors first.
    pub expr: Vec<Expr<'a>>,

    /// Lists all errors that occurred during parsing.
    ///
    /// This may be empty if the parsing process succeeded.
    pub errors: Vec<ParseError>,
}

impl<'a> ParseReport<'a> {
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn is_err(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn expect(self, msg: &str) -> Vec<Expr<'a>> {
        if self.is_ok() {
            self.expr
        } else {
            panic!("{msg} {:?}", self.errors)
        }
    }

    pub fn unwrap(self) -> Vec<Expr<'a>> {
        if self.is_ok() {
            self.expr
        } else {
            panic!("ParseReport contains errors: {:?}", self.errors)
        }
    }
}

impl<'a> From<ParseResult<Vec<Expr<'a>>>> for ParseReport<'a> {
    fn from(result: ParseResult<Vec<Expr<'a>>>) -> Self {
        match result {
            Ok(expr) => Self {
                expr,
                errors: Vec::new(),
            },
            Err(err) => Self {
                expr: Vec::new(),
                errors: vec![err],
            },
        }
    }
}

impl<'a> From<ParseReport<'a>> for ParseResult<Vec<Expr<'a>>> {
    fn from(mut report: ParseReport<'a>) -> Self {
        if report.is_err() {
            Err(report.errors.remove(0))
        } else {
            Ok(report.expr)
        }
    }
}

/// A list of byte offsets to skip.
///
/// Even indexes are start byte offsets, odd indexes are end byte offsets.
#[derive(Debug)]
pub(crate) struct SkipSections(Vec<usize>);

impl SkipSections {
    /// Returns `true` if the given offset is contained in a skip section.
    pub(crate) fn contains(&self, offset: usize) -> bool {
        let pos = self
            .0
            .binary_search(&offset)
            .map(|pos| pos + 1)
            .unwrap_or_else(|pos| pos);
        pos & 1 == 1
    }
}

/// Determines each section to skip.
pub(crate) fn determine_skip_sections(
    input_len: usize,
    unmatched: &[UnmatchedDelimiter],
) -> SkipSections {
    let mut skip_sections = Vec::new();
    for unmatched in unmatched {
        if let Some(candidate) = unmatched.candidate {
            skip_sections.push(candidate);
        } else {
            break;
        }
        if let Some(closing) = unmatched.closing {
            skip_sections.push(closing);
        } else {
            skip_sections.push(input_len + 1); // Matches unclosed delimiters at EOF.
            break;
        }
    }
    SkipSections(skip_sections)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_skip_sections() {
        let skip = SkipSections(vec![]);
        assert!(!skip.contains(0));
    }

    #[test]
    fn one_skip_section() {
        let skip = SkipSections(vec![3, 9]);
        assert!(!skip.contains(0));
        assert!(!skip.contains(2));
        assert!(skip.contains(3));
        assert!(skip.contains(5));
        assert!(skip.contains(8));
        assert!(!skip.contains(9));
        assert!(!skip.contains(10));
    }
}
