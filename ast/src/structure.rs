use crate::Expr;
use dbg_pls::DebugPls;

/// A constructor call.
///
/// This differs from a function call in that it creates a new instance of a
/// type, rather than perform a specific task. Constructors are always named
/// and are always called with parentheses.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Construct<'a> {
    /// The name of the structure to construct.
    pub name: &'a str,

    /// The arguments to pass to the constructor.
    pub args: Vec<Expr<'a>>,
}
