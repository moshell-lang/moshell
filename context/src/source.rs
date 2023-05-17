use miette::{MietteError, MietteSpanContents, SourceCode, SourceSpan, SpanContents};
use std::fmt::Debug;

pub type SourceSegment = std::ops::Range<usize>;

pub trait SourceSegmentHolder {
    fn segment(&self) -> SourceSegment;
}

pub struct StaticSegmentHolder {
    segment: SourceSegment
}

impl SourceSegmentHolder for StaticSegmentHolder {
    fn segment(&self) -> SourceSegment {
        self.segment.clone()
    }
}

impl StaticSegmentHolder {
    pub fn new(segment: SourceSegment) -> Self {
        Self { segment }
    }
}

impl From<SourceSegment> for StaticSegmentHolder {
    fn from(value: SourceSegment) -> Self {
        StaticSegmentHolder::new(value)
    }
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
            self.name.to_owned(),
            contents.data(),
            *contents.span(),
            contents.line(),
            contents.column(),
            contents.line_count(),
        )))
    }
}

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
