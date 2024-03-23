use std::fmt::Debug;

#[cfg(feature = "miette")]
use miette::{MietteError, MietteSpanContents, SourceCode, SourceSpan, SpanContents};

/// A range of bytes in an unbound string source.
pub type SourceSegment = std::ops::Range<usize>;

/// An identifier to a source code.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ContentId(pub usize);

pub trait SourceSegmentHolder {
    fn segment(&self) -> SourceSegment;
}

/// Defines a named source code from which tokens can be produced.
#[derive(Clone, Copy)]
pub struct Source<'a> {
    /// The source code.
    pub source: &'a str,
    /// The name of the source code.
    pub name: &'a str,
}

impl<'a> Source<'a> {
    /// Creates a new named source from a string.
    pub fn new(source: &'a str, name: &'a str) -> Self {
        Self { source, name }
    }

    /// Creates a new source of unknown origin.
    ///
    /// This value should normally be used only for quick testing.
    pub fn unknown(source: &'a str) -> Self {
        Self::new(source, "unknown")
    }

    /// Gets the length in bytes of the source.
    pub fn len(&self) -> usize {
        self.source.len()
    }

    /// Tests if the source is empty.
    pub fn is_empty(&self) -> bool {
        self.source.is_empty()
    }
}

#[cfg(feature = "miette")]
impl<'b> SourceCode for Source<'b> {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        let contents = self
            .source
            .read_span(span, context_lines_before, context_lines_after)?;

        Ok(Box::new(MietteSpanContents::new_named(
            self.name.to_owned(),
            contents.data(),
            *contents.span(),
            contents.line(),
            contents.column(),
            contents.line_count(),
        )))
    }
}

impl Debug for Source<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Source")
            .field("name", &self.name)
            .field("source", &"<redacted>")
            .finish()
    }
}

impl SourceSegmentHolder for Source<'_> {
    fn segment(&self) -> SourceSegment {
        0..self.source.len()
    }
}

impl SourceSegmentHolder for &str {
    fn segment(&self) -> SourceSegment {
        0..self.len()
    }
}

#[derive(Clone)]
pub struct OwnedSource {
    pub source: String,
    pub name: String,
}

impl OwnedSource {
    pub fn new(source: String, name: String) -> Self {
        Self { source, name }
    }

    pub fn as_source(&self) -> Source {
        Source::new(&self.source, &self.name)
    }
}
