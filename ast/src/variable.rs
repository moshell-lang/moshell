use compact_str::CompactString;
use std::fmt::Display;

use context::source::{SourceSegment, SourceSegmentHolder};
use lexer::token::TokenType;
use src_macros::segment_holder;

use crate::r#type::Type;
use crate::r#use::InclusionPathItem;
use crate::Expr;

/// A variable declaration.
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclaration {
    /// The kind of the variable.
    pub kind: VarKind,
    /// The declaration.
    pub var: TypedVariable,
    /// The value of the variable to be evaluated.
    pub initializer: Option<Box<Expr>>,
}

/// A named variable declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct TypedVariable {
    /// The name of the variable.
    pub name: Identifier,
    /// The type of the declared variable.
    pub ty: Option<Type>,
}

impl SourceSegmentHolder for TypedVariable {
    fn segment(&self) -> SourceSegment {
        if let Some(ty) = &self.ty {
            self.name.segment().start..ty.segment().end
        } else {
            self.name.segment()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VarKind {
    Var,
    Val,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarName {
    /// An used defined variable name.
    User(CompactString),

    /// The `self` keyword to refer to the current object.
    Slf,
}

impl VarName {
    pub fn name(&self) -> &str {
        match self {
            VarName::User(name) => name,
            VarName::Slf => "self",
        }
    }
}

/// A variable reference, prefixed with `$`.
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct VarReference {
    /// The name of the variable.
    pub name: VarName,
}

/// A variable assignation.
#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    /// The place that is assigned to.
    pub left: Box<Expr>,
    /// The operator of the assignation.
    pub operator: AssignOperator,
    /// The value of the variable to be evaluated.
    pub value: Box<Expr>,
}

/// An assignation operator.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AssignOperator {
    Assign,
    Increment,
    Decrement,
    Multiply,
    Divide,
    Remainder,
}

/// A path identifier, that do not start with `$`.
#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    pub path: Vec<InclusionPathItem>,
}

impl SourceSegmentHolder for Path {
    fn segment(&self) -> SourceSegment {
        self.path[0].segment().start..self.path.last().unwrap().segment().end
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.path.iter();
        if let Some(first) = iter.next() {
            write!(f, "{}", first)?;
            for item in iter {
                write!(f, "::{}", item)?;
            }
        }
        Ok(())
    }
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

impl SourceSegmentHolder for Assign {
    fn segment(&self) -> SourceSegment {
        self.left.segment().start..self.value.segment().end
    }
}

impl Assign {
    pub fn name(&self) -> Option<String> {
        match self.left.as_ref() {
            Expr::Path(ident) => Some(ident.to_string()),
            Expr::VarReference(VarReference { name, .. }) => Some(name.name().to_owned()),
            _ => None,
        }
    }
}

/// An expansion of an expression related to directories.
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct TildeExpansion {
    pub structure: Tilde,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Tilde {
    HomeDir(Option<Box<Expr>>),
    WorkingDir,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub value: CompactString,
    pub start: usize,
}

impl Identifier {
    pub fn new(value: CompactString, start: usize) -> Self {
        Self { value, start }
    }

    pub fn extract(text: &str, span: SourceSegment) -> Self {
        let start = span.start;
        Self {
            value: CompactString::new(&text[span]),
            start,
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.value, f)
    }
}

impl SourceSegmentHolder for Identifier {
    fn segment(&self) -> SourceSegment {
        self.start..self.start + self.value.len()
    }
}
