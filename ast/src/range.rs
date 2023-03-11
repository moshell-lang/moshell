use crate::variable::VarReference;
use crate::Expr;
use dbg_pls::DebugPls;

/// A range of values that can be iterated over.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Iterable<'a> {
    Range(NumericRange<'a>),
    Files(FilePattern<'a>),
    Var(VarReference<'a>),
}

/// A range of numeric values.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct NumericRange<'a> {
    /// The inclusive start of the range.
    pub start: Expr<'a>,

    /// The end of the range.
    ///
    /// See `upper_inclusive` for whether this is inclusive or exclusive.
    pub end: Expr<'a>,

    /// The step of the range.
    pub step: Option<Expr<'a>>,

    /// Whether the upper bound is inclusive or exclusive.
    pub upper_inclusive: bool,
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
}
