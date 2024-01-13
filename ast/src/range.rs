use context::source::{SourceSegment, SourceSegmentHolder};
use src_macros::segment_holder;

use crate::Expr;

/// A range of values that can be iterated over.
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
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
        let start = self.start.segment().start;
        let end = self.step.as_ref().unwrap_or(&self.end).segment().end;

        start..end
    }
}

/// A pattern that can be used to match files.
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct FilePattern<'a> {
    /// The glob pattern that will be used to match files.
    pub pattern: Box<Expr<'a>>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct Subscript<'a> {
    pub target: Box<Expr<'a>>,
    pub index: Box<Expr<'a>>,
}
