use miette::{MietteError, MietteSpanContents, SourceCode, SourceSpan, SpanContents};
use std::fmt::Debug;

pub type SourceSegment = std::ops::Range<usize>;

pub trait SourceSegmentHolder {
    fn segment(&self) -> SourceSegment;
}

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

    pub fn len(&self) -> usize {
        self.source.len()
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
pub fn try_join<'a, T>(owner: &'a [T], a: &'a [T], b: &'a [T]) -> Option<&'a [T]> {
    let owner_head = owner.as_ptr();
    let owner_tail = owner[owner.len()..].as_ptr();
    let a_head = a.as_ptr();
    let b_tail = b[b.len()..].as_ptr();
    if owner_head <= a_head && a_head <= b_tail && b_tail <= owner_tail {
        // SAFETY: The two slices are adjacent in memory and are owned by the same container.
        Some(unsafe { std::slice::from_raw_parts(a.as_ptr(), b_tail as usize - a_head as usize) })
    } else {
        None
    }
}

/// Joins two string slices that are adjacent in memory into one string slice.
///
/// Returns None in the case the slices aren't adjacent.
pub fn try_join_str<'a>(owner: &'a str, a: &'a str, b: &'a str) -> Option<&'a str> {
    // SAFETY: The two slices are already valid UTF-8.
    try_join(owner.as_bytes(), a.as_bytes(), b.as_bytes())
        .map(|s| unsafe { std::str::from_utf8_unchecked(s) })
}
