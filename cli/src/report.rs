use std::io;
use std::io::Write;

use analyzer::diagnostic::Diagnostic;
use analyzer::engine::Engine;
use miette::{LabeledSpan, MietteDiagnostic, Report, Severity, SourceSpan};

use analyzer::reef::{Externals, ReefId};
use context::source::{ContentId, Source, SourceSegment};
use parser::err::{ParseError, ParseErrorKind};

use crate::pipeline::{SourceHolder, SourcesCache};

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
        }
        _ => {}
    }
    write_diagnostic(diag, Some(source), writer)
}

pub fn display_diagnostic<W: Write>(
    externals: &Externals,
    current_engine: &Engine,
    engine_reef: ReefId,
    sources: &SourcesCache,
    diagnostic: Diagnostic,
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

    if let Some((head, tail)) = diagnostic.helps.split_first() {
        if tail.is_empty() {
            diag = diag.with_help(head)
        }
        let helps = tail.iter().fold(format!("\n- {head}"), |acc, help| {
            format!("{acc}\n- {help}")
        });
        diag = diag.with_help(helps)
    }

    struct AttachedSource<'a> {
        reef: ReefId,
        id: ContentId,
        content: Source<'a>,
    }

    let mut displayed_source: Option<AttachedSource> = None;
    for obs in diagnostic.observations {
        let loc = obs.location;
        let engine = if engine_reef == loc.reef {
            current_engine
        } else {
            &externals.get_reef(loc.reef).unwrap().engine
        };

        let content_id = engine
            .get_original_content(loc.source)
            .expect("Unknown source");

        if displayed_source
            .as_ref()
            .map_or(true, |s| s.id == content_id && s.reef == loc.reef)
        {
            let source = sources
                .get(loc.reef)
                .and_then(|importer| importer.get_source(content_id))
                .expect("Unknown source");
            let span = loc.segment.clone();
            diag = diag.and_label(LabeledSpan::new(obs.message, span.start, span.len()));
            displayed_source = Some(AttachedSource {
                reef: loc.reef,
                id: content_id,
                content: source,
            });
        }
    }

    write_diagnostic(diag, displayed_source.map(|s| s.content), writer)
}

fn write_diagnostic<W: Write>(
    diagnostic: MietteDiagnostic,
    source: Option<Source>,
    writer: &mut W,
) -> io::Result<()> {
    let report = Report::from(diagnostic);
    if let Some(source) = source {
        unsafe {
            //SAFETY: the CLI source is transmuted to a static lifetime, because `report.with_source_code`
            // needs a source with a static lifetime.
            // The report and the source are then used to display the formatted diagnostic and are immediately dropped after.
            let source = std::mem::transmute::<Source, Source<'static>>(source);
            let report = report.with_source_code(source);
            writeln!(writer, "\n{report:?}")
        }
    } else {
        writeln!(writer, "\n{report:?}")
    }
}
