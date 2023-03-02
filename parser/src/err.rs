use crate::ast::Expr;
use crate::parser::ParseResult;
use crate::source::Location;

/// An error that occurs during parsing.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub position: Location,
    pub kind: ParseErrorKind,
}

/// The kind of error that occurred.
///
/// This is used to categorize the error and to provide more information
/// when available.
#[derive(Debug, PartialEq, Clone)]
pub enum ParseErrorKind {
    /// A specific token was expected, but another token was found.
    Excepted(&'static str),

    /// A unexpected token was found in the current context.
    Unexpected,

    /// A group has been opened, but not closed.
    ///
    /// This reports the location of the opening token.
    Unpaired(Location),

    /// A token cannot be parsed.
    NotParsable,
}

/// The parsing result.
///
/// This contains all the parsed expressions and the errors that occurred
/// during parsing. The parser will try to recover from errors and continue
/// parsing, but always assume that if there is any error, the whole parsing
/// process have failed.
#[derive(Debug, PartialEq)]
pub struct ParseReport<'a> {
    pub expr: Vec<Expr<'a>>,
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
            panic!("{} {:?}", msg, self.errors)
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
