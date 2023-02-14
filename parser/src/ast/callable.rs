use lexer::token::Token;
use crate::ast::Expr;
use crate::ast::variable::TypedVariable;

/// A call to a function or a command.
#[derive(Debug, Clone, PartialEq)]
pub struct Call<'a> {
    /// The name of the function or command.
    pub name: Token<'a>,
    /// The arguments of the function or command.
    pub arguments: Vec<Expr<'a>>,
}

/// A function declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct FunDeclaration<'a> {
    pub name: Token<'a>,
    pub parameters: Vec<TypedVariable<'a>>,
    pub body: Vec<Expr<'a>>,
}
