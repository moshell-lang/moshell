use context::source::{SourceSegment, SourceSegmentHolder};
use src_macros::segment_holder;

use crate::r#type::{Type, TypeParameter};
use crate::variable::{Identifier, TypedVariable};
use crate::Expr;

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub expr: Option<Box<Expr>>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub type_parameters: Vec<TypeParameter>,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Option<Type>,
    pub body: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionParameter {
    Named(TypedVariable),
    ///argument is the type of the variable (if any).
    Variadic(Option<Type>, SourceSegment),
    Slf(SourceSegment),
}

impl SourceSegmentHolder for FunctionParameter {
    fn segment(&self) -> SourceSegment {
        match self {
            FunctionParameter::Named(n) => n.segment(),
            FunctionParameter::Variadic(_, s) => s.clone(),
            FunctionParameter::Slf(s) => s.clone(),
        }
    }
}
