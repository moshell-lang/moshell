use crate::reef::ReefId;
use ast::call::{RedirFd, RedirOp};
use ast::value::LiteralValue;
use context::source::{SourceSegment, SourceSegmentHolder};

use crate::relations::{LocalId, ResolvedSymbol, SourceId};
use crate::types::engine::{FunctionId, StructureId};
use crate::types::ty::TypeRef;
use crate::types::ERROR;

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub enum Var {
    Local(LocalId),
    External(ResolvedSymbol),
}

/// A type checked expression attached to a source segment.
#[derive(Clone, Debug, PartialEq)]
pub struct TypedExpr {
    pub kind: ExprKind,
    pub ty: TypeRef,
    pub segment: SourceSegment,
}

impl SourceSegmentHolder for TypedExpr {
    fn segment(&self) -> SourceSegment {
        self.segment.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldAccess {
    pub object: Box<TypedExpr>,
    pub structure: StructureId,
    pub structure_reef: ReefId,
    pub field: LocalId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldAssign {
    pub object: Box<TypedExpr>,
    pub structure: StructureId,
    pub structure_reef: ReefId,
    pub field: LocalId,
    pub new_value: Box<TypedExpr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LocalAssignment {
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
    pub into: TypeRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Loop {
    pub condition: Option<Box<TypedExpr>>,
    pub body: Box<TypedExpr>,
}

/// A for loop.
#[derive(Debug, Clone, PartialEq)]
pub struct ForLoop {
    /// The type of the for loop.
    pub kind: Box<ForKind>,
    /// The body of the for loop.
    pub body: Box<TypedExpr>,
}

/// A for loop can be either a range loop or a conditional loop.
#[derive(Debug, Clone, PartialEq)]
pub enum ForKind {
    Range(RangeFor),
    Conditional(ConditionalFor),
}

/// A for in range loop, e.g. `for i in 1..10; ...`.
#[derive(Debug, Clone, PartialEq)]
pub struct RangeFor {
    /// The variable name that will be used in the loop to designate the current item.
    pub receiver: LocalId,

    /// The type of the receiver.
    pub receiver_type: TypeRef,

    /// The range of values that will be iterated over.
    pub iterable: TypedExpr,
}

/// A for in conditional loop, e.g. `for (( i = 0; i < 10; i++ )); ...`.
#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalFor {
    /// The initialization expression.
    pub initializer: TypedExpr,
    /// The condition expression.
    pub condition: TypedExpr,
    /// The increment expression.
    pub increment: TypedExpr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub arguments: Vec<TypedExpr>,
    pub reef: ReefId,
    pub function_id: FunctionId,
    pub source_id: Option<SourceId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MethodCall {
    pub callee: Box<TypedExpr>,
    pub arguments: Vec<TypedExpr>,
    pub function_id: FunctionId,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Substitute {
    In(Vec<TypedExpr>),
    Out(Vec<TypedExpr>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Subprocess {
    pub inner: Box<TypedExpr>,
    pub awaited: bool,
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
    LocalAssign(LocalAssignment),
    FieldAssign(FieldAssign),
    Declare(Declaration),
    Reference(Var),
    FieldAccess(FieldAccess),
    Block(Vec<TypedExpr>),
    Redirect(Redirect),
    Conditional(Conditional),
    ConditionalLoop(Loop),
    ForLoop(ForLoop),
    Convert(Convert),
    ProcessCall(Vec<TypedExpr>),
    FunctionCall(FunctionCall),
    MethodCall(MethodCall),
    Return(Option<Box<TypedExpr>>),
    Pipeline(Vec<TypedExpr>),
    Capture(Vec<TypedExpr>),
    Substitute(Substitute),
    Subprocess(Subprocess),

    Continue,
    Break,
    Noop,
}

impl TypedExpr {
    /// Creates a no-op expression that describes an error.
    pub(crate) fn error(segment: SourceSegment) -> Self {
        Self {
            kind: ExprKind::Noop,
            ty: ERROR,
            segment,
        }
    }

    /// Sets the type of the expression to [`crate::types::ty::Type::Error`].
    pub(crate) fn poison(mut self) -> Self {
        self.ty = ERROR;
        self
    }
}
