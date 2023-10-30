use dbg_pls::DebugPls;

use context::source::{SourceSegment, SourceSegmentHolder};
use lexer::token::TokenType;
use src_macros::segment_holder;

use crate::r#type::Type;
use crate::Expr;

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

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum VarName<'a> {
    /// An used defined variable name.
    User(&'a str),

    /// The `self` keyword to refer to the current object.
    Slf,
}

impl VarName<'_> {
    pub fn name(&self) -> &str {
        match self {
            VarName::User(name) => name,
            VarName::Slf => "self",
        }
    }
}

/// A variable reference, prefixed with `$`.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct VarReference<'a> {
    /// The name of the variable.
    pub name: VarName<'a>,
}

/// A variable assignation.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Assign<'a> {
    /// The place that is assigned to.
    pub left: Box<Expr<'a>>,
    /// The operator of the assignation.
    pub operator: AssignOperator,
    /// The value of the variable to be evaluated.
    pub value: Box<Expr<'a>>,
}

/// An assignation operator.
#[derive(Debug, Clone, Copy, PartialEq, DebugPls)]
pub enum AssignOperator {
    Assign,
    Increment,
    Decrement,
    Multiply,
    Divide,
    Remainder,
}

/// A simple identifier, that do not start with `$`.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Identifier<'a> {
    pub name: &'a str,
}

impl TryFrom<TokenType> for AssignOperator {
    type Error = ();

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenType::Equal => Self::Assign,
            TokenType::Plus => Self::Increment,
            TokenType::Minus => Self::Decrement,
            TokenType::Star => Self::Multiply,
            TokenType::Slash => Self::Divide,
            TokenType::Percent => Self::Remainder,
            _ => return Err(()),
        })
    }
}

impl SourceSegmentHolder for Assign<'_> {
    fn segment(&self) -> SourceSegment {
        self.left.segment().start..self.value.segment().end
    }
}

impl Assign<'_> {
    pub fn name(&self) -> Option<&str> {
        match self.left.as_ref() {
            Expr::Identifier(Identifier { name, .. }) => Some(name),
            Expr::VarReference(VarReference { name, .. }) => Some(name.name()),
            _ => None,
        }
    }
}

/// An expansion of an expression related to directories.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct TildeExpansion<'a> {
    pub structure: Tilde<'a>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Tilde<'a> {
    HomeDir(Option<Box<Expr<'a>>>),
    WorkingDir,
}
