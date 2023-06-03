use crate::relations::ObjectId;
use ast::operation::BinaryOperator;
use ast::value::LiteralValue;
use context::source::{SourceSegment, SourceSegmentHolder};

/// A type identifier in a [`Typing`] instance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub ObjectId);

/// A type checked expression attached to a source segment.
#[derive(Debug, PartialEq)]
pub struct TypedExpr {
    pub(crate) kind: ExprKind,
    pub(crate) ty: TypeId,
    pub(crate) segment: SourceSegment,
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
        name: String,
        value: Option<Box<TypedExpr>>,
    },
    Reference {
        name: String,
    },
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
}
