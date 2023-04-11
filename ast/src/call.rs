use crate::r#type::Type;
use crate::Expr;
use dbg_pls::DebugPls;

/// A raw call to a function or a command.
///
/// This call is highly permissive since it allows for any expression as the
/// command name.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Call<'a> {
    ///The relative path where this _function_ is stored.
    pub path: Vec<&'a str>,

    /// The arguments of the command.
    ///
    /// A valid command must have at least one argument that is the command name.
    pub arguments: Vec<Expr<'a>>,

    /// type parameters of the call.
    pub type_parameters: Vec<Type<'a>>,
}

/// A programmatic call.
///
/// Theses always have a constant name and are always called with parentheses.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct ProgrammaticCall<'a> {
    /// inclusion path
    pub path: Vec<&'a str>,

    /// The name of the function to call.
    pub name: &'a str,

    /// The arguments to pass to the function.
    pub arguments: Vec<Expr<'a>>,

    /// The type parameters of the call.
    pub type_parameters: Vec<Type<'a>>,
}

/// A call to a function or a command.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Detached<'a> {
    /// The arguments of the command.
    ///
    /// A valid command must have at least one argument that is the command name.
    pub underlying: Box<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Redirected<'a> {
    pub expr: Box<Expr<'a>>,
    pub redirections: Vec<Redir<'a>>,
}

/// A redirection.
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

/// Redirection operators.
///
/// This enum defines the redirection operator types except here-document and
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
