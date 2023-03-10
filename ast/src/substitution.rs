use crate::group::Subshell;
use dbg_pls::DebugPls;

/// A special type of grouping expression that should be substituted
/// based on its expression and kind.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Substitution<'a> {
    pub underlying: Subshell<'a>,
    pub kind: SubstitutionKind,
}

/// The kind of substitution that should be performed.
#[derive(Debug, Clone, Copy, PartialEq, DebugPls)]
pub enum SubstitutionKind {
    /// An arithmetic evaluation with `$((...))`.
    Arithmetic,
    /// A command standard output substitution with `$(...)`.
    Capture,
}
