mod report;

use crate::report::print_flush;
use context::source::{InputSource, Location, StringSource};
use dbg_pls::color;
use miette::{Diagnostic, GraphicalReportHandler, SourceSpan};
use parser::err::ParseErrorKind;
use parser::parse;
use std::fmt::Display;
use std::io::{self, BufRead, Write};
use thiserror::Error;
use lexer::reader::BufferedTokenReader;

#[derive(Error, Debug, Diagnostic)]
struct FormattedError<'s, S> {
    #[source_code]
    src: &'s StringSource,
    #[label("Here")]
    cursor: SourceSpan,
    #[label("Start")]
    related: Option<SourceSpan>,
    message: String,
    #[help]
    help: Option<String>,
}

impl<S> Display for FormattedError<'_, S> {
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
    while let Some(line) = lines.next() {
        let first_line = line?;
        let mut source = StringSource::new(first_line, "cli");

        let reader = BufferedTokenReader::from_line_supplier(|| {
            print_flush!("..");
            let result = lines.next();
            if let Ok(Some(ln)) = result {
                source.append_code(ln);
            }
            result
        });

        let report = parse(reader);

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
                    ParseErrorKind::Excepted(excepted) => Some(format!("Excepted: {:?}", excepted)),
                    ParseErrorKind::UnexpectedInContext(help) => Some(help.clone()),
                    _ => None,
                },
            })
            .collect::<Vec<_>>();

        if errors.is_empty() {
            print_flush!("{}\n=> ", color(&report.expr));
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
