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
    // the type this expression was implicitly converted to
    pub implicit_cast: Option<TypeId>,
    pub segment: SourceSegment,
}

impl SourceSegmentHolder for TypedExpr {
    fn segment(&self) -> SourceSegment {
        self.segment.clone()
    }
}

/// An expression content.
#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Literal(LiteralValue),
    Assign {
        lhs: Box<TypedExpr>,
        rhs: Box<TypedExpr>,
    },
    Declare {
        identifier: LocalId,
        value: Option<Box<TypedExpr>>,
    },
    Reference(Symbol),
    Binary {
        lhs: Box<TypedExpr>,
        op: BinaryOperator,
        rhs: Box<TypedExpr>,
    },
    Block(Vec<TypedExpr>),
    Conditional {
        condition: Box<TypedExpr>,
        then: Box<TypedExpr>,
        otherwise: Option<Box<TypedExpr>>,
    },
    ProcessCall(Vec<TypedExpr>),
    FunctionCall {
        name: String,
        arguments: Vec<TypedExpr>,
    },
    Return(Option<Box<TypedExpr>>),
    Noop,
}
