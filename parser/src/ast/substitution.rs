use crate::ast::group::Parenthesis;

/// A special type of grouping expression that should be substituted
/// based on its expression and kind.
#[derive(Debug, Clone, PartialEq)]
pub struct Substitution<'a> {
    pub underlying: Parenthesis<'a>,
    pub kind: SubstitutionKind,
}

/// The kind of substitution that should be performed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SubstitutionKind {
    /// An arithmetic evaluation with `$((...))`.
    Arithmetic,
    /// A command standard output substitution with `$(...)`.
    Capture,
    /// A return value substitution with `@(...)`.
    Return,
}
