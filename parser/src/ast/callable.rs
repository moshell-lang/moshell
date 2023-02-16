use crate::ast::variable::TypedVariable;
use crate::ast::Expr;
use lexer::token::Token;

/// A call to a function or a command.
#[derive(Debug, Clone, PartialEq)]
pub struct Call<'a> {
    /// The arguments of the command.
    ///
    /// A valid command must have at least one argument that is the command name.
    pub arguments: Vec<Expr<'a>>,
    /// The redirections of the command.
    pub redirections: Vec<Redir<'a>>,
}

/// A function declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct FunDeclaration<'a> {
    pub name: Token<'a>,
    pub parameters: Vec<TypedVariable<'a>>,
    pub body: Vec<Expr<'a>>,
}

/// A redirection.
#[derive(Debug, Clone, PartialEq)]
pub struct Redir<'a> {
    /// File descriptor that is modified by this redirection.
    pub fd: RedirFd,
    /// The nature of the redirection.
    pub operator: RedirOp,
    /// The file name or file descriptor to redirect to.
    pub operand: Expr<'a>,
}

/// A file descriptor that is redirected.
#[derive(Debug, Copy, Clone, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct Pipeline<'a> {
    /// Elements of the pipeline.
    ///
    /// A valid pipeline must have at least one command.
    pub commands: Vec<Call<'a>>,
    /// True if the pipeline begins with a `!`.
    pub negation: bool,
}

/// Redirection operators.
///
/// This enum defines the redirection operator types except here-document and
/// process redirection.
#[derive(Clone, Copy, Debug, PartialEq)]
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
