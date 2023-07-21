use ast::call::{RedirFd, RedirOp};
use ast::value::LiteralValue;
use context::source::{SourceSegment, SourceSegmentHolder};

use crate::relations::{Definition, LocalId, ObjectId, ResolvedSymbol};
use crate::types::{ERROR, NOTHING};

/// A type identifier in a [`Typing`] instance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub ObjectId);

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub enum Var {
    Local(LocalId),
    Capture(ResolvedSymbol),
}

impl TypeId {
    pub fn is_nothing(self) -> bool {
        self == NOTHING
    }

    pub fn is_something(self) -> bool {
        self != NOTHING
    }

    pub fn is_ok(self) -> bool {
        self != ERROR
    }

    pub fn is_err(self) -> bool {
        self == ERROR
    }
}

/// A type checked expression attached to a source segment.
#[derive(Clone, Debug, PartialEq)]
pub struct TypedExpr {
    pub kind: ExprKind,
    pub ty: TypeId,
    pub segment: SourceSegment,
}

impl SourceSegmentHolder for TypedExpr {
    fn segment(&self) -> SourceSegment {
        self.segment.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub identifier: Var,
    pub rhs: Box<TypedExpr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub identifier: LocalId,
    pub value: Option<Box<TypedExpr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Conditional {
    pub condition: Box<TypedExpr>,
    pub then: Box<TypedExpr>,
    pub otherwise: Option<Box<TypedExpr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Convert {
    pub inner: Box<TypedExpr>,
    pub into: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Loop {
    pub condition: Option<Box<TypedExpr>>,
    pub body: Box<TypedExpr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub arguments: Vec<TypedExpr>,
    pub definition: Definition,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MethodCall {
    pub callee: Box<TypedExpr>,
    pub arguments: Vec<TypedExpr>,
    pub definition: Definition,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Redirect {
    pub expression: Box<TypedExpr>,
    pub redirections: Vec<Redir>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Redir {
    pub fd: RedirFd,
    pub operator: RedirOp,
    pub operand: Box<TypedExpr>,
}

/// An expression content.
#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Literal(LiteralValue),
    Assign(Assignment),
    Declare(Declaration),
    Reference(Var),
    Block(Vec<TypedExpr>),
    Redirect(Redirect),
    Conditional(Conditional),
    ConditionalLoop(Loop),
    Convert(Convert),
    ProcessCall(Vec<TypedExpr>),
    FunctionCall(FunctionCall),
    MethodCall(MethodCall),
    Return(Option<Box<TypedExpr>>),
    Pipeline(Vec<TypedExpr>),
    Capture(Vec<TypedExpr>),

    Continue,
    Break,
    Noop,
}
