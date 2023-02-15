use crate::ast::Expr;
use lexer::token::Token;

/// A typed variable.
#[derive(Debug, Clone, PartialEq)]
pub struct TypedVariable<'a> {
    /// The name of the variable.
    pub name: Token<'a>,
    /// The type of the variable.
    pub ty: Option<Token<'a>>,
}

/// A variable declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclaration<'a> {
    /// The kind of the variable.
    pub kind: VarKind,
    /// The variable.
    pub var: TypedVariable<'a>,
    /// The value of the variable to be evaluated.
    pub initializer: Option<Box<Expr<'a>>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VarKind {
    Var,
    Val,
}

/// A variable reference, prefixed with `$`.
#[derive(Debug, Clone, PartialEq)]
pub struct VarReference<'a> {
    /// The name of the variable.
    pub name: Token<'a>,
}

/// A variable assignation.
#[derive(Debug, Clone, PartialEq)]
pub struct Assign<'a> {
    /// The identifier of the variable.
    pub name: Token<'a>,
    /// The value of the variable to be evaluated.
    pub value: Box<Expr<'a>>,
}
