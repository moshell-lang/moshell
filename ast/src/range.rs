use crate::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};
use dbg_pls::DebugPls;

/// A range of values that can be iterated over.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Iterable<'a> {
    Range(NumericRange<'a>),
    Files(FilePattern<'a>),
}

impl SourceSegmentHolder for Iterable<'_> {
    fn segment(&self) -> SourceSegment {
        match self {
            Iterable::Range(range) => range.segment(),
            Iterable::Files(pattern) => pattern.segment.clone(),
        }
    }
}

/// A range of numeric values.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct NumericRange<'a> {
    /// The inclusive start of the range.
    pub start: Box<Expr<'a>>,

    /// The end of the range.
    ///
    /// See `upper_inclusive` for whether this is inclusive or exclusive.
    pub end: Box<Expr<'a>>,

    /// The step of the range.
    pub step: Option<Box<Expr<'a>>>,

    /// Whether the upper bound is inclusive or exclusive.
    pub upper_inclusive: bool,
}

impl SourceSegmentHolder for NumericRange<'_> {
    fn segment(&self) -> SourceSegment {
        self.start.segment().start..self.step.as_ref().unwrap_or(&self.end).segment().end
    }
}

/// A pattern that can be used to match files.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct FilePattern<'a> {
    /// The raw glob pattern that was used to create this pattern.
    pub lexeme: &'a str,

    /// The glob pattern that will be used to match files.
    ///
    /// For now, this is just a string that is passed to the libc.
    pub pattern: String,

    /// The segment of the source code that this pattern was created from.
    pub segment: SourceSegment,
}
