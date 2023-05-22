use std::io;
use std::io::Write;

use miette::{LabeledSpan, MietteDiagnostic, Report, Severity, SourceSpan};

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

fn offset_empty_span(span: SourceSegment) -> SourceSpan {
    if span.start == span.end {
        (span.start - 1..span.end).into()
    } else {
        span.into()
    }
}

pub fn display_parse_error<W: Write>(
    source: Source,
    error: ParseError,
    writer: &mut W,
) -> io::Result<()> {
    let span = offset_empty_span(error.position);
    let mut diag = MietteDiagnostic::new(error.message)
        .with_severity(Severity::Error)
        .with_code("error".to_string());

    match error.kind {
        ParseErrorKind::Expected(e) => {
            diag = diag.and_label(LabeledSpan::new(Some(e), span.offset(), span.len()))
        }
        ParseErrorKind::UnexpectedInContext(e) => {
            diag = diag.and_label(LabeledSpan::new(Some(e), span.offset(), span.len()))
        }
        ParseErrorKind::Unpaired(e) => {
            let unpaired_span = offset_empty_span(e);
            diag = diag.and_label(LabeledSpan::new(
                Some("Start".to_string()),
                unpaired_span.offset(),
                unpaired_span.len(),
            ));
            diag = diag.and_label(LabeledSpan::new(
                Some("Here".to_string()),
                span.offset(),
                span.len(),
            ))
        }
        _ => {
            diag = diag.and_label(LabeledSpan::new(
                Some("Here".to_string()),
                span.offset(),
                span.len(),
            ))
        }
    }
    let report = Report::from(diag).with_source_code(source.source.to_string());
    writeln!(writer, "{report:?}")
}

pub fn display_diagnostic<W: Write>(
    source: Source,
    diagnostic: analyzer::diagnostic::Diagnostic,
    writer: &mut W,
) -> io::Result<()> {
    let mut diag = MietteDiagnostic::new(diagnostic.global_message);

    let id = diagnostic.identifier;
    diag = if id.noxious() {
        diag.with_severity(Severity::Error)
            .with_code(format!("error[{}]", id.code()))
    } else {
        diag.with_severity(Severity::Warning)
            .with_code(format!("warn[{}]", id.code()))
    };

    if let Some((head, tail)) = diagnostic.tips.split_first() {
        if tail.is_empty() {
            diag = diag.with_help(head)
        }
        let helps = tail.iter().fold(format!("\n- {head}"), |acc, help| {
            format!("{acc}\n- {help}")
        });
        diag = diag.with_help(helps)
    }

    for obs in diagnostic.observations {
        diag = diag.and_label(LabeledSpan::new(
            obs.help,
            obs.segment.start,
            obs.segment.len(),
        ))
    }

    let report = Report::from(diag);
    let report = report.with_source_code(source.source.to_string());
    writeln!(writer, "\n{report:?}")
}
