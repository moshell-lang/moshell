use crate::typing::registry::{FunctionId, SchemaId};
use crate::typing::user::{TypeId, ERROR_TYPE, UNIT_TYPE, UNKNOWN_TYPE};
use crate::typing::variable::{LocalEnvironment, LocalId, Var};
use crate::Reef;
use ast::call::{RedirFd, RedirOp};
use ast::value::LiteralValue;
use context::source::Span;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct TypedExpr {
    pub kind: ExprKind,
    pub ty: TypeId,
    pub span: Span,
}

impl TypedExpr {
    pub fn noop(span: Span) -> Self {
        Self {
            kind: ExprKind::Noop,
            span,
            ty: UNIT_TYPE,
        }
    }

    pub fn error(span: Span) -> Self {
        Self {
            kind: ExprKind::Noop,
            span,
            ty: ERROR_TYPE,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldAccess {
    pub object: Box<TypedExpr>,
    pub structure: SchemaId,
    pub field: LocalId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldAssign {
    pub object: Box<TypedExpr>,
    pub structure: SchemaId,
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
    pub identifier: Var,
    pub value: Option<Box<TypedExpr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Conditional {
    pub condition: Box<TypedExpr>,
    pub then: Box<TypedExpr>,
    pub otherwise: Option<Box<TypedExpr>>,
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
    pub receiver_type: TypeId,

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
    pub function_id: FunctionId,
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
    ProcessCall(Vec<TypedExpr>),
    FunctionCall(FunctionCall),
    MethodCall(MethodCall),
    Return(Option<Box<TypedExpr>>),
    Pipeline(Vec<TypedExpr>),
    Capture(Vec<TypedExpr>),
    Substitute(Substitute),
    Subprocess(Subprocess),
    Cast(Box<TypedExpr>),

    Continue,
    Break,
    Noop,
}

impl TypedExpr {
    pub(crate) fn is_ok(&self) -> bool {
        !self.is_err()
    }

    pub(crate) fn is_err(&self) -> bool {
        matches!(self.ty, UNKNOWN_TYPE | ERROR_TYPE)
    }
}

/// A unit of code.
pub struct Chunk {
    /// The fully qualified name to access this chunk.
    pub fqn: PathBuf,

    pub function: Option<FunctionId>,

    /// The expression that this chunk represents.
    pub expr: TypedExpr,

    pub locals: LocalEnvironment,
}

/// A group of [`Chunk`]s that were defined in the same context (usually a file).
pub struct Module {
    fqn: PathBuf,
    chunks: Vec<Chunk>,
    pub(crate) exports: HashMap<Rc<String>, TypeId>,
}

impl Module {
    pub(crate) fn new(fqn: PathBuf) -> Self {
        Self {
            fqn,
            chunks: Vec::new(),
            exports: HashMap::new(),
        }
    }

    pub(crate) fn enter_namespace(&mut self, name: &str) {
        self.fqn.push(name);
    }

    pub(crate) fn exit_namespace(&mut self) {
        self.fqn.pop();
    }

    pub(crate) fn add(
        &mut self,
        function: Option<FunctionId>,
        expr: TypedExpr,
        locals: LocalEnvironment,
    ) {
        self.chunks.push(Chunk {
            fqn: self.fqn.clone(),
            function,
            expr,
            locals,
        });
    }
}

impl Reef {
    pub fn group_by_content(&self) -> ContentIterator {
        ContentIterator {
            inner: self.hir.as_slice().iter(),
        }
    }
}

pub type NamedExports = HashMap<Rc<String>, TypeId>;

/// A group of chunks that were defined in the same content.
#[derive(Copy, Clone)]
pub struct EncodableContent<'a> {
    /// The main chunk of this content.
    pub main: &'a Chunk,

    /// The functions that this content provides.
    pub functions: &'a [Chunk],

    /// The exports that this content provides.
    pub exports: &'a NamedExports,
}

pub struct ContentIterator<'a> {
    inner: std::slice::Iter<'a, Module>,
}

impl<'a> Iterator for ContentIterator<'a> {
    type Item = EncodableContent<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(
            |Module {
                 fqn: _,
                 chunks,
                 exports,
             }| {
                let (main, functions) = chunks.split_last().unwrap();
                EncodableContent {
                    main,
                    functions,
                    exports,
                }
            },
        )
    }
}
