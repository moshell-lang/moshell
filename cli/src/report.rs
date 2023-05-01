use context::source::{Source, SourceSegment};
use miette::{Diagnostic, SourceSpan};
use parser::err::{ParseError, ParseErrorKind};
use std::fmt::Display;
use thiserror::Error;

macro_rules! print_flush {
    ( $($t:tt)* ) => {
        {
            let mut stdout = std::io::stdout();
            write!(stdout, $($t)* ).unwrap();
            stdout.flush().unwrap();
        }
    }
}

pub(crate) use print_flush;

#[derive(Error, Debug, Diagnostic)]
pub struct FormattedError<'s> {
    #[source_code]
    src: &'s Source<'s>,
    #[label("Here")]
    cursor: SourceSpan,
    #[label("Start")]
    related: Option<SourceSpan>,
    message: String,
    #[help]
    help: Option<String>,
}

impl Display for FormattedError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

fn offset_empty_span(span: SourceSegment) -> SourceSpan {
    if span.start == span.end {
        (span.start - 1..span.end).into()
    } else {
        span.into()
    }
}

impl<'a> FormattedError<'a> {
    pub fn from(err: ParseError, source: &'a Source<'a>) -> Self {
        Self {
            src: source,
            cursor: offset_empty_span(err.position),
            related: match &err.kind {
                ParseErrorKind::Unpaired(pos) => Some(pos.clone().into()),
                _ => None,
            },
            message: err.message,
            help: match &err.kind {
                ParseErrorKind::Expected(expected) => Some(format!("Expected: {:?}", expected)),
                ParseErrorKind::UnexpectedInContext(help) => Some(help.clone()),
                _ => None,
            },
        }
    }
}
