use miette::{MietteError, MietteSpanContents, SourceCode, SourceSpan, SpanContents};
use std::fmt::Debug;

pub type Location = std::ops::Range<usize>;

/// Defines a named source code from which tokens can be produced.
#[derive(Clone)]
pub struct Source<'a> {
    /// The source code.
    pub source: &'a str,
    /// The name of the source code.
    pub name: String,
}

impl<'a> Source<'a> {
    /// Creates a new named source from a string.
    pub fn new(source: &'a str, name: impl Into<String>) -> Self {
        Self {
            source,
            name: name.into(),
        }
    }

    /// Creates a new source of unknown origin.
    ///
    /// This value should normally be used only for quick testing.
    pub fn unknown(source: &'a str) -> Self {
        Self {
            source,
            name: "unknown".to_string(),
        }
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

impl<'s> SourceCode for &'s Source<'_> {
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
            self.name.clone(),
            contents.data(),
            *contents.span(),
            contents.line(),
            contents.column(),
            contents.line_count(),
        )))
    }
}

/// Joins two slices that are adjacent in memory into one slice.
///
/// Returns None in the case the slices aren't adjacent.
pub fn try_join<'a, T>(a: &'a [T], b: &'a [T]) -> Option<&'a [T]> {
    let a_len = a.len();
    let a_tail = a[a_len..].as_ptr();
    if std::ptr::eq(a_tail, b.as_ptr()) {
        // SAFETY: The two slices are adjacent in memory.
        Some(unsafe { std::slice::from_raw_parts(a.as_ptr(), a.len() + b.len()) })
    } else {
        None
    }
}

/// Joins two string slices that are adjacent in memory into one string slice.
///
/// Returns None in the case the slices aren't adjacent.
pub fn try_join_str<'a>(a: &'a str, b: &'a str) -> Option<&'a str> {
    // SAFETY: The two slices are already valid UTF-8.
    try_join(a.as_bytes(), b.as_bytes()).map(|s| unsafe { std::str::from_utf8_unchecked(s) })
}
