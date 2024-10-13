use std::path::PathBuf;

use analyzer::import::PathEntry;
use analyzer::symbol::SymbolDesc;
use analyzer::typing::{ErroneousSymbolDesc, TypeError, TypeErrorKind};
use analyzer::{Filesystem, PipelineError, SourceLocation};
use context::source::Span;
use miette::{LabeledSpan, MietteDiagnostic, Severity, SourceOffset, SourceSpan};

pub fn error_to_diagnostic(
    value: PipelineError,
    multi_file: &mut MultiFile,
    fs: &dyn Filesystem,
) -> MietteDiagnostic {
    match value {
        PipelineError::Import { path, error, cause } => {
            let mut diagnostic =
                MietteDiagnostic::new(format!("unable to import {}: {error}", path.display()));
            if let Some(SourceLocation { path, span }) = cause {
                let span = multi_file.insert(path, span, fs);
                diagnostic = diagnostic.with_label(LabeledSpan::new_with_span(None, span))
            }
            diagnostic
        }
        PipelineError::Parse { path, error } => {
            let span = multi_file.insert(path, error.position, fs);
            MietteDiagnostic::new(error.message)
                .with_severity(Severity::Error)
                .and_label(LabeledSpan::new_with_span(Some("Here".to_string()), span))
        }
        PipelineError::Type(error) => type_error_to_diagnostic(error, multi_file, fs),
    }
}

fn type_error_to_diagnostic(
    TypeError { kind, at }: TypeError,
    multi_file: &mut MultiFile,
    fs: &dyn Filesystem,
) -> MietteDiagnostic {
    let at_span = multi_file.insert(at.path.clone(), at.span, fs);
    let mut diagnostic = MietteDiagnostic::new(kind.to_string())
        .with_label(LabeledSpan::new_with_span(None, at_span));
    match kind {
        TypeErrorKind::DuplicateSymbol { previous, .. } => {
            let previous_span = multi_file.insert(at.path, previous, fs);
            diagnostic.and_label(LabeledSpan::new_with_span(
                Some("previous declaration here".to_owned()),
                previous_span,
            ))
        }
        TypeErrorKind::UndefinedSymbol {
            name,
            expected,
            found: Some(desc),
        } => {
            let entry = PathEntry::from(&desc);
            diagnostic.message = format!("expected {expected}, found {entry} `{name}`");
            if let ErroneousSymbolDesc::Complete(SymbolDesc { registry, span }) = desc {
                let span = multi_file.insert(at.path, span, fs);
                diagnostic = diagnostic.with_label(LabeledSpan::new_with_span(
                    Some(format!("{registry} defined here")),
                    span,
                ))
            }
            diagnostic
        }
        TypeErrorKind::TypeMismatch {
            expected_due_to: Some(expected_due_to),
            ..
        } => {
            let expected_span = multi_file.insert(expected_due_to.path, expected_due_to.span, fs);
            diagnostic.and_label(LabeledSpan::new_with_span(
                Some("expected here".to_owned()),
                expected_span,
            ))
        }
        TypeErrorKind::UnknownField { available, .. } if !available.is_empty() => diagnostic
            .with_help(format!(
                "Available fields: {}",
                available.into_iter().collect::<Vec<_>>().join(", ")
            )),
        TypeErrorKind::TypeAnnotationRequired { types, insert_at } => {
            let span = multi_file.insert(at.path, insert_at..insert_at, fs);
            diagnostic.with_label(LabeledSpan::new_with_span(
                Some(format!("::[{}]", types.join(", "))),
                span,
            ))
        }
        TypeErrorKind::RepeatedParameterName { name, previous } => {
            let previous_span = multi_file.insert(at.path, previous, fs);
            diagnostic.and_label(LabeledSpan::new_with_span(
                Some(format!("previous declaration of `{name}`")),
                previous_span,
            ))
        }
        TypeErrorKind::MethodLikeFieldAccess { name, parentheses } => diagnostic.with_help(
            format!("use parentheses to call the method: .{name}{parentheses}",),
        ),
        _ => diagnostic,
    }
}

#[derive(Default)]
pub struct MultiFile {
    sources: Vec<VirtualFile>,
}

struct VirtualFile {
    name: PathBuf,
    source: String,
}

impl MultiFile {
    pub fn insert(&mut self, path: PathBuf, span: Span, fs: &dyn Filesystem) -> SourceSpan {
        let mut start = 0usize;
        for source in &self.sources {
            if source.name == path {
                return SourceSpan::new(SourceOffset::from(start + span.start), span.len());
            } else {
                start += source.source.len();
            }
        }
        let Ok(source) = fs.read(&path) else {
            panic!("unable to re-read file: {}", path.display());
        };
        self.sources.push(VirtualFile {
            name: path,
            source: source.to_string(),
        });
        SourceSpan::new(SourceOffset::from(start + span.start), span.len())
    }
}

impl miette::SourceCode for MultiFile {
    fn read_span<'b>(
        &'b self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'b> + 'b>, miette::MietteError> {
        let mut start = 0usize;
        for file in &self.sources {
            if start + file.source.len() <= span.offset() {
                start += file.source.len();
                continue;
            }
            let local_span = SourceSpan::new(SourceOffset::from(span.offset() - start), span.len());
            let contents =
                file.source
                    .read_span(&local_span, context_lines_before, context_lines_after)?;
            let local_span = contents.span();
            let span = SourceSpan::new(
                SourceOffset::from(local_span.offset() + start),
                local_span.len(),
            );
            return Ok(Box::new(miette::MietteSpanContents::new_named(
                file.name.to_string_lossy().to_string(),
                contents.data(),
                span,
                contents.line(),
                contents.column(),
                contents.line_count(),
            )));
        }
        Err(miette::MietteError::OutOfBounds)
    }
}
