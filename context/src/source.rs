use miette::{MietteError, MietteSpanContents, SourceCode, SourceSpan, SpanContents};
use std::fmt::Debug;
use std::io;
use std::io::{BufRead, Lines};
use lexer::reader::BufferedTokenReader;

pub type Location = std::ops::Range<usize>;

trait Source {
    fn source_code(&self) -> String;

    fn name(&self) -> String;
}

/// Defines a named source code from which tokens can be produced.
#[derive(Clone)]
pub struct StringSource {
    /// The source code.
    source: String,
    /// The source code name.
    source_name: String,
}


///String source implementation
impl StringSource {
    /// Creates a new named source from a string.
    pub fn new(source: &str, name: impl Into<String>) -> Self {
        Self {
            source: source.to_string(),
            source_name: name.into(),
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
}

impl Debug for dyn Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Source")
            .field("name", &self.name())
            .field("source", &"<redacted>")
            .finish()
    }
}

impl<'s> SourceCode for dyn Source + Send + Sync {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        let contents = self
            .source_code()
            .read_span(span, context_lines_before, context_lines_after)?;
        Ok(Box::new(MietteSpanContents::new_named(
            self.name().clone(),
            contents.data(),
            *contents.span(),
            contents.line(),
            contents.column(),
            contents.line_count(),
        )))
    }
}

struct InputSource<I> {
    input: I,
    source_code: String,
    source_name: String,
}

///input string source implementation for BufRead input
impl<'a, B: BufRead> InputSource<B> {
    pub fn from_lines(input: B, name: impl Into<String>) -> Self {
        Self {
            input,
            source_code: String::new(),
            source_name: name.into(),
        }
    }

    pub fn reader(&'a mut self) -> BufferedTokenReader<impl FnMut() -> Result<Option<&'a str>, io::Error>> {
        let x = || self.read_and_append();
        BufferedTokenReader::from_line_supplier(x)
    }

    fn read_and_append(&'a mut self) -> Result<Option<&'a str>, io::Error> {
        let line_start = self.source_name.len();
        self.input.read_line(&mut self.source_code)?;
        Ok(Some(&self.source_code[line_start..]))
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
