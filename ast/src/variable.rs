use crate::r#type::Type;
use crate::Expr;
use dbg_pls::DebugPls;
use src_macros::segment_holder;

/// A variable declaration.
#[segment_holder]
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
#[segment_holder]
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
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct VarReference<'a> {
    /// The name of the variable.
    pub name: &'a str,
}

/// A variable assignation.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Assign<'a> {
    /// The identifier of the variable.
    pub name: &'a str,
    /// The value of the variable to be evaluated.
    pub value: Box<Expr<'a>>,
}
