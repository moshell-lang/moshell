use std::fmt;
use std::fmt::{Debug, Display, Error, Formatter};

use miette::{Diagnostic, GraphicalReportHandler, LabeledSpan, Severity, SourceCode};

use context::source::Source;

use crate::cli::offset_empty_span;

#[derive(Debug)]
struct FormattedAnalyzerDiagnostic<'a> {
    code: String,
    severity: Severity,
    help: Option<String>,
    source_code: Source<'a>,
    labels: Vec<LabeledSpan>,
    message: String,
}

impl<'a> Display for FormattedAnalyzerDiagnostic<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for FormattedAnalyzerDiagnostic<'_> {}

impl<'a> Diagnostic for FormattedAnalyzerDiagnostic<'a> {
    fn code<'b>(&'b self) -> Option<Box<dyn Display + 'b>> {
        Some(Box::new(&self.code))
    }

    fn severity(&self) -> Option<Severity> {
        Some(self.severity)
    }

    fn help<'b>(&'b self) -> Option<Box<dyn Display + 'b>> {
        self.help.as_ref().map(|s| Box::new(s) as Box<dyn Display>)
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        Some(&self.source_code)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(self.labels.iter().cloned()))
    }
}

pub fn render_diagnostic(
    source_code: Source,
    diagnostic: analyzer::diagnostic::Diagnostic,
    handler: &GraphicalReportHandler,
) -> Result<String, Error> {
    let labels = diagnostic
        .observations
        .into_iter()
        .map(|obs| {
            let span = offset_empty_span(obs.segment);
            LabeledSpan::new(obs.help, span.offset(), span.len())
        })
        .collect();

    let id = diagnostic.identifier;
    let (code, severity) = if id.critical() {
        (format!("error[E{:04}]", id.code()), Severity::Error)
    } else {
        (format!("warn[W{:04}]", id.code()), Severity::Warning)
    };

    let mut help = None;
    if let Some((head, tail)) = diagnostic.helps.split_first() {
        help = if tail.is_empty() {
            Some(head.clone())
        } else {
            let helps_agg = tail.iter().fold(format!("\n- {head}"), |acc, help| {
                format!("{acc}\n- {help}")
            });
            Some(helps_agg)
        }
    }

    let diag = FormattedAnalyzerDiagnostic {
        code,
        source_code,
        labels,
        severity,
        help,
        message: diagnostic.global_message,
    };
    let mut str = String::new();
    handler.render_report(&mut str, &diag).map(|_| str)
}
