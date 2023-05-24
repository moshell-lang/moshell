use context::source::Source;
use miette::{MietteError, MietteSpanContents, SourceCode, SourceSpan, SpanContents};

pub struct CLISourceCode<'a> {
    pub(crate) source: Source<'a>,
}

pub fn into_source_code(source: Source) -> CLISourceCode {
    CLISourceCode { source }
}

impl<'b> SourceCode for CLISourceCode<'b> {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        let contents =
            self.source
                .source
                .read_span(span, context_lines_before, context_lines_after)?;
        Ok(Box::new(MietteSpanContents::new_named(
            self.source.name.to_owned(),
            contents.data(),
            *contents.span(),
            contents.line(),
            contents.column(),
            contents.line_count(),
        )))
    }
}
