use crate::relations::{ObjectId, Symbol};
use crate::types::ty::Definition;
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
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Literal(LiteralValue),
    Assign {
        identifier: Symbol,
        rhs: Box<TypedExpr>,
    },
    Declare {
        identifier: ObjectId,
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
    Convert {
        inner: Box<TypedExpr>,
        into: TypeId,
    },
    ProcessCall(Vec<TypedExpr>),
    FunctionCall {
        arguments: Vec<TypedExpr>,
        definition: Definition,
    },
    MethodCall {
        callee: Box<TypedExpr>,
        arguments: Vec<TypedExpr>,
        definition: Definition,
    },
    Return(Option<Box<TypedExpr>>),
    Noop,
}
