use crate::Expr;
use dbg_pls::DebugPls;
use crate::r#type::Type;

/// A variable declaration.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct VarDeclaration<'a> {
    /// The kind of the variable.
    pub kind: VarKind,
    /// The declaration.
    pub var: TypedVariable<'a>,
    /// The value of the variable to be evaluated.
    pub initializer: Option<Box<Expr<'a>>>,
}

/// A named variable declaration.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct TypedVariable<'a> {
    /// The name of the variable.
    pub name: &'a str,
    /// The type of the declared variable.
    pub ty: Option<Type<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, DebugPls)]
pub enum VarKind {
    Var,
    Val,
}

/// A variable reference, prefixed with `$`.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct VarReference<'a> {
    /// The name of the variable.
    pub name: &'a str,
}

/// A variable assignation.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Assign<'a> {
    /// The identifier of the variable.
    pub name: &'a str,
    /// The value of the variable to be evaluated.
    pub value: Box<Expr<'a>>,
}
