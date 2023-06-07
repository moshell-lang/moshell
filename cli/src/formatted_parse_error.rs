use std::fmt;
use std::fmt::Display;

use miette::{Diagnostic, GraphicalReportHandler, SourceSpan};
use thiserror::Error;

use context::source::Source;
use parser::err::{ParseError, ParseErrorKind};

use crate::cli::offset_empty_span;

#[derive(Error, Debug, Diagnostic)]
struct FormattedParseError<'s> {
    message: String,
    #[source_code]
    src: Source<'s>,
    #[label("Here")]
    cursor: SourceSpan,
    #[label("Start")]
    related: Option<SourceSpan>,
    #[help]
    help: Option<String>,
}

impl Display for FormattedParseError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn render_parse_error(
    error: ParseError,
    handler: &GraphicalReportHandler,
    source: Source,
) -> Result<String, fmt::Error> {
    let err = FormattedParseError {
        message: error.message,
        src: source,
        cursor: offset_empty_span(error.position),
        related: match &error.kind {
            ParseErrorKind::Unpaired(pos) => Some(pos.clone().into()),
            _ => None,
        },
        help: match &error.kind {
            ParseErrorKind::Expected(expected) => Some(format!("Expected: {expected:?}")),
            ParseErrorKind::UnexpectedInContext(help) => Some(help.clone()),
            _ => None,
        },
    };

    let mut buff = String::new();

    handler.render_report(&mut buff, &err).map(|_| buff)
}
