use crate::r#type::Type;
use crate::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};
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
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct TypedVariable<'a> {
    /// The name of the variable.
    pub name: &'a str,
    /// The type of the declared variable.
    pub ty: Option<Type<'a>>,

    pub segment: SourceSegment,
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
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Assign<'a> {
    /// The identifier of the variable.
    pub name: &'a str,
    /// The value of the variable to be evaluated.
    pub value: Box<Expr<'a>>,
}

impl<'a> SourceSegmentHolder for Assign<'a> {
    fn segment(&self) -> SourceSegment {
        self.name..self.value.segment().end
    }
}
