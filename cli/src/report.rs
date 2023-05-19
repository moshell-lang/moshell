use std::fmt::{Debug, Display};
use std::io;
use std::io::{Write};

use miette::{Diagnostic, LabeledSpan, MietteDiagnostic, Report, Severity, SourceSpan};
use thiserror::Error;

use analyzer::diagnostic::DiagnosticType::{Error, Warn};
use context::source::{Source, SourceSegment};
use parser::err::{ParseError, ParseErrorKind};

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
pub struct FormattedParseError<'s> {
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

impl Display for FormattedParseError<'_> {
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

impl<'a> FormattedParseError<'a> {
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
                ParseErrorKind::Expected(expected) => Some(format!("Expected: {expected:?}")),
                ParseErrorKind::UnexpectedInContext(help) => Some(help.clone()),
                _ => None,
            },
        }
    }
}

pub fn display_diagnostic<W>(source: Source,
                             diagnostic: analyzer::diagnostic::Diagnostic,
                             writer: &mut W) -> io::Result<()> where W: Write {
    let mut builder = MietteDiagnostic::new(diagnostic.global_message);

    builder = match diagnostic.ty {
        Warn(w) => builder.with_severity(Severity::Warning)
            .with_code(format!("warn[{}]", w.code())),
        Error(e) => builder.with_severity(Severity::Error)
            .with_code(format!("error[{}]", e.code())),
    };

    if let Some((head, tail)) = diagnostic.tips.split_first() {
        if tail.is_empty() {
            builder = builder.with_help(head)
        }
        let helps = tail
            .into_iter()
            .fold(format!("\n- {head}"), |acc, help| format!("{acc}\n- {help}"));
        builder = builder.with_help(helps)
    }

    for obs in diagnostic.observations {
        builder = builder.and_label(LabeledSpan::new(obs.help, obs.segment.start, obs.segment.len()))
    }

    let report = Report::from(builder);
    let report = report.with_source_code(source.source.to_string());
    writeln!(writer, "\n{report:?}")
}
