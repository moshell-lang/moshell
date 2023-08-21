use dbg_pls::DebugPls;

use context::source::{SourceSegment, SourceSegmentHolder};
use src_macros::segment_holder;

use crate::r#type::Type;
use crate::r#use::InclusionPathItem;
use crate::Expr;

/// A raw call to a function or a command.
///
/// This call is highly permissive since it allows for any expression as the
/// command name.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Call<'a> {
    /// The arguments of the command.
    ///
    /// A valid command must have at least one argument which is the command name.
    pub arguments: Vec<Expr<'a>>,
}

impl SourceSegmentHolder for Call<'_> {
    fn segment(&self) -> SourceSegment {
        // A call must have at least one argument.
        self.arguments.first().unwrap().segment().start
            ..self.arguments.last().unwrap().segment().end
    }
}

/// A programmatic call.
///
/// Theses always have a constant name and are always called with parentheses.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct ProgrammaticCall<'a> {
    /// inclusion path, with the function's name
    pub path: Vec<InclusionPathItem<'a>>,

    /// The arguments to pass to the function.
    pub arguments: Vec<Expr<'a>>,

    /// The type parameters of the call.
    pub type_parameters: Vec<Type<'a>>,
}

/// A method call.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct MethodCall<'a> {
    /// The expression on which the method is called.
    pub source: Box<Expr<'a>>,

    /// The name of the method to call.
    ///
    /// The name cannot be an expression, so it is always a constant string after the parsing phase.
    pub name: Option<&'a str>,

    /// The arguments to pass to the method.
    pub arguments: Vec<Expr<'a>>,

    /// The type parameters of the call.
    pub type_parameters: Vec<Type<'a>>,
}

/// A call to a function or a command.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Detached<'a> {
    /// The arguments of the command.
    ///
    /// A valid command must have at least one argument that is the command name.
    pub underlying: Box<Expr<'a>>,
}

/// An expression with IO redirections.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Redirected<'a> {
    /// The expression to redirect.
    pub expr: Box<Expr<'a>>,
    /// The redirections to apply to the expression.
    ///
    /// A valid redirected expression must have at least one redirection.
    pub redirections: Vec<Redir<'a>>,
}

impl SourceSegmentHolder for Redirected<'_> {
    fn segment(&self) -> SourceSegment {
        // A redirected expression must have at least one redirection.
        self.expr.segment().start..self.redirections.last().unwrap().operand.segment().end
    }
}

/// A redirection.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Redir<'a> {
    /// File descriptor that is modified by this redirection.
    pub fd: RedirFd,
    /// The nature of the redirection.
    pub operator: RedirOp,
    /// The file name or file descriptor to redirect to.
    pub operand: Expr<'a>,
}

/// A file descriptor that is redirected.
#[derive(Debug, Copy, Clone, PartialEq, DebugPls)]
pub enum RedirFd {
    /// The default file descriptor, if not specified.
    ///
    /// This often means to use the standard input or output.
    Default,
    /// All the standard output file descriptors, grouped with `&`.
    Wildcard,
    /// A specific file descriptor.
    Fd(u32),
}

/// Commands separated by `|`
#[derive(Clone, Debug, PartialEq, DebugPls)]
pub struct Pipeline<'a> {
    /// Elements of the pipeline.
    ///
    /// A valid pipeline must have at least one command.
    pub commands: Vec<Expr<'a>>,
}

impl SourceSegmentHolder for Pipeline<'_> {
    fn segment(&self) -> SourceSegment {
        // A pipeline must have at least one command.
        self.commands.first().unwrap().segment().start..self.commands.last().unwrap().segment().end
    }
}

/// Redirection operators.
///
/// This enum defines the redirection operator type except here-document and
/// process redirection.
#[derive(Clone, Copy, Debug, PartialEq, DebugPls)]
pub enum RedirOp {
    /// Open a file for input (`<`)
    Read,
    /// Open a file for input and output (`<>`)
    ReadWrite,
    /// Open a file for output; clear if existing (`>`)
    Write,
    /// Open a file for output; append if existing (`>>`)
    Append,
    /// Duplicate a file descriptor for input (`<&`)
    FdIn,
    /// Duplicate a file descriptor for output (`>&`)
    FdOut,
    /// Here-string (`<<<`)
    String,
}
