use std::fmt::Debug;

use miette::{MietteError, MietteSpanContents, SourceCode, SourceSpan, SpanContents};

pub type Location = std::ops::Range<usize>;


/// Defines a named source code from which tokens can be produced.
#[derive(Clone)]
pub struct Source {
    /// The source code.
    pub source: String,
    /// The source code name.
    pub source_name: String,
}

///String source implementation
impl Source {
    /// Creates a new named source from a string.
    pub fn new(source: &str, name: impl Into<String>) -> Self {
        Self {
            source: source.to_string(),
            source_name: name.into(),
        }
    }

    pub fn empty(name: impl Into<String>) -> Self {
        Self {
            source: String::new(),
            source_name: name.into()
        }
    }

    /// Creates a new source of unknown origin.
    ///
    /// This value should normally be used only for quick testing.
    pub fn unknown(source: &str) -> Self {
        Self {
            source: source.to_string(),
            source_name: "unknown".to_string(),
        }
    }

    pub fn append_code(&mut self, str: &str) {
        self.source.push_str(str)
    }


    /// Get the relative byte offset of the given string in the source code.
    ///
    /// # Panics
    /// This method panics if the given string is not contained in the source code.
    pub fn relative_pos(&self, str: &str) -> Location {
        let start = (str.as_ptr() as usize)
            .checked_sub(self.source.as_ptr() as usize)
            .expect("String is not contained in the source code.");
        let end = start + str.len();
        start..end
    }

    /// Get the relative byte offset of the given context in the source code.
    ///
    /// # Panics
    /// This method panics if the given context is not contained in the source code.
    pub fn relative_pos_ctx(&self, context: impl Into<ErrorContext<'a>>) -> Location {
        let context = context.into();
        let start = (context.from.as_ptr() as usize)
            .checked_sub(self.source.as_ptr() as usize)
            .expect("Context start is not contained in the source code.");
        let end = context.to.as_ptr() as usize + context.to.len() as usize
            - self.source.as_ptr() as usize;
        start..end
    }
    
}

impl<'a> Debug for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Source")
            .field("name", &self.source_name)
            .field("source", &"<redacted>")
            .finish()
    }
}

impl<'s> SourceCode for Source {
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
            self.source_name.to_string(),
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
