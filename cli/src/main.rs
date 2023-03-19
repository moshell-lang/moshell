mod report;

use crate::report::print_flush;
use context::source::{Location, Source};
use dbg_pls::color;
use miette::{Diagnostic, GraphicalReportHandler, SourceSpan};
use parser::err::ParseErrorKind;
use parser::parse;
use std::fmt::Display;
use std::io::{self, BufRead, Write};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
struct FormattedError<'s> {
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

fn offset_empty_span(span: Location) -> SourceSpan {
    if span.start == span.end {
        (span.start - 1..span.end).into()
    } else {
        span.into()
    }
}

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();

    let handler = GraphicalReportHandler::default();

    print_flush!("=> ");
    let mut content = String::new();
    while let Some(line) = lines.next() {
        let line = line?;
        content.push_str(&line);
        if line.ends_with('\\') {
            content.push('\n');
            print_flush!(".. ");
            continue;
        }

        let source = Source::new(&content, "stdin");
        let report = parse(source.clone());
        if !report.stack_ended {
            content.push('\n');
            print_flush!(".. ");
            continue; // Silently ignore incomplete input
        }

        let errors = report
            .errors
            .into_iter()
            .map(|err| FormattedError {
                src: &source,
                cursor: offset_empty_span(err.position),
                related: match &err.kind {
                    ParseErrorKind::Unpaired(pos) => Some(pos.clone().into()),
                    _ => None,
                },
                message: err.message,
                help: match &err.kind {
                    ParseErrorKind::Expected(excepted) => Some(format!("Expected: {:?}", excepted)),
                    ParseErrorKind::UnexpectedInContext(help) => Some(help.clone()),
                    _ => None,
                },
            })
            .collect::<Vec<_>>();

        if errors.is_empty() {
            if !&report.expr.is_empty() {
                print_flush!("{}\n=> ", color(&report.expr));
            } else {
                print_flush!("=> ");
            }
            content.clear();
            continue;
        }

        let mut msg = String::new();
        for err in &errors {
            if let Err(fmt_err) = handler.render_report(&mut msg, err) {
                eprintln!("{fmt_err}");
            } else {
                eprintln!("{msg}");
            }
            msg.clear();
        }
        content.clear();
        print_flush!("=> ");
    }

    Ok(())
}
