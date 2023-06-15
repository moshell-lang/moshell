use crate::relations::{LocalId, ObjectId, Symbol};
use crate::types::{ERROR, NOTHING};
use ast::operation::BinaryOperator;
use ast::value::LiteralValue;
use context::source::{SourceSegment, SourceSegmentHolder};

/// A type identifier in a [`Typing`] instance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub ObjectId);

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
#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct Assignment {
    pub lhs: Box<TypedExpr>,
    pub rhs: Box<TypedExpr>,
}

#[derive(Debug, PartialEq)]
pub struct Declaration {
    pub identifier: LocalId,
    pub value: Option<Box<TypedExpr>>,
}

#[derive(Debug, PartialEq)]
pub struct Binary {
    pub lhs: Box<TypedExpr>,
    pub op: BinaryOperator,
    pub rhs: Box<TypedExpr>,
}

#[derive(Debug, PartialEq)]
pub struct Conditional {
    pub condition: Box<TypedExpr>,
    pub then: Box<TypedExpr>,
    pub otherwise: Option<Box<TypedExpr>>,
}

#[derive(Debug, PartialEq)]
pub struct Loop {
    pub condition: Option<Box<TypedExpr>>,
    pub body: Box<TypedExpr>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<TypedExpr>,
}

/// An expression content.
#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Literal(LiteralValue),
    Assign(Assignment),
    Declare(Declaration),
    Reference(Symbol),
    Binary(Binary),
    Block(Vec<TypedExpr>),
    Conditional(Conditional),
    ConditionalLoop(Loop),
    ProcessCall(Vec<TypedExpr>),
    FunctionCall(FunctionCall),
    Return(Option<Box<TypedExpr>>),

    Continue,
    Break,
    Noop,
}
