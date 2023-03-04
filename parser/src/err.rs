use crate::ast::Expr;
use crate::parser::ParseResult;
use context::source::Location;
use lexer::token::Token;

/// An error that occurs during parsing.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub position: Location,
    pub kind: ParseErrorKind,
}

/// A builder to position an error that covers multiple tokens.
#[derive(Debug, PartialEq)]
pub(crate) struct ErrorContext<'a> {
    pub(crate) from: Token<'a>,
    pub(crate) to: Token<'a>,
}

impl<'a> From<Token<'a>> for ErrorContext<'a> {
    fn from(token: Token<'a>) -> Self {
        Self {
            from: token.clone(),
            to: token,
        }
    }
}

impl<'a> From<std::ops::Range<Token<'a>>> for ErrorContext<'a> {
    fn from(range: std::ops::Range<Token<'a>>) -> Self {
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
    Excepted(&'static str),

    /// A unexpected token was found in the current context.
    Unexpected,

    /// A group has been opened, but not closed.
    ///
    /// This reports the location of the opening token.
    Unpaired(Location),

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

    /// Indicates that the end of the input has been reached too early and
    /// that the parser is waiting for more input.
    ///
    /// This flag is set only when a block delimiter is opened, but not closed,
    /// and there was no actual error in the input.
    pub stack_ended: bool,
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
                stack_ended: true,
            },
            Err(err) => Self {
                expr: Vec::new(),
                errors: vec![err],
                stack_ended: true,
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
