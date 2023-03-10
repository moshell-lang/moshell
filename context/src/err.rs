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

impl<'a> From<&'a str> for ErrorContext<'a> {
    fn from(str: &'a str) -> Self {
        Self { from: str, to: str }
    }
}