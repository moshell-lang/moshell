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
        .and_label(LabeledSpan::new(
            Some("Here".to_string()),
            span.offset(),
            span.len(),
        ));

    match error.kind {
        ParseErrorKind::Expected(e) => diag = diag.with_help(format!("expected: {e}")),
        ParseErrorKind::UnexpectedInContext(e) => diag = diag.with_help(e),
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
        _ => {}
    }
    write_diagnostic(diag, source, writer)
}

pub fn display_diagnostic<W: Write>(
    source: Source,
    diagnostic: analyzer::diagnostic::Diagnostic,
    writer: &mut W,
) -> io::Result<()> {
    let mut diag = MietteDiagnostic::new(diagnostic.global_message);

    let id = diagnostic.identifier;
    diag = if id.critical() {
        diag.with_severity(Severity::Error)
            .with_code(format!("error[E{:04}]", id.code()))
    } else {
        diag.with_severity(Severity::Warning)
            .with_code(format!("warn[W{:04}]", id.code()))
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

    write_diagnostic(diag, source, writer)
}

fn write_diagnostic<W: Write>(
    diagnostic: MietteDiagnostic,
    source: Source,
    writer: &mut W,
) -> io::Result<()> {
    let report = Report::from(diagnostic);
    unsafe {
        //SAFETY: the CLI source is transmuted to a static lifetime, because `report.with_source_code`
        //needs a source with a static lifetime. The report and the source are then used to display the formatted diagnostic and are immediately dropped after.
        let source = std::mem::transmute::<Source, Source<'static>>(source);
        let report = report.with_source_code(source);
        writeln!(writer, "\n{report:?}")
    }
}
