use context::source::{SourceSegment, SourceSegmentHolder};

use crate::group::Subshell;

/// A special type of grouping expression that should be substituted
/// based on its expression and kind.
#[derive(Debug, Clone, PartialEq)]
pub struct Substitution<'a> {
    pub underlying: Subshell<'a>,
    pub kind: SubstitutionKind,
}

impl SourceSegmentHolder for Substitution<'_> {
    fn segment(&self) -> SourceSegment {
        self.underlying.segment.clone()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Direction {
    Input,
    Output,
}

/// The kind of substitution that should be performed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SubstitutionKind {
    /// A command standard output substitution with `$(...)`.
    Capture,

    /// A process substitution with `<(...)` or `>(...)`.
    Process { direction: Direction },
}
