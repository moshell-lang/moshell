use context::source::{SourceSegment, SourceSegmentHolder};
use dbg_pls::DebugPls;

use src_macros::segment_holder;

use crate::r#type::{Type, TypeParameter};
use crate::variable::TypedVariable;
use crate::Expr;

#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Return<'a> {
    pub expr: Option<Box<Expr<'a>>>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct FunctionDeclaration<'a> {
    pub name: &'a str,
    pub type_parameters: Vec<TypeParameter<'a>>,
    pub parameters: Vec<FunctionParameter<'a>>,
    pub return_type: Option<Type<'a>>,
    pub body: Box<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum FunctionParameter<'a> {
    Named(TypedVariable<'a>),
    ///argument is the type of the variable (if any).
    Variadic(Option<Type<'a>>, SourceSegment),
    Slf(SourceSegment),
}

impl SourceSegmentHolder for FunctionParameter<'_> {
    fn segment(&self) -> SourceSegment {
        match self {
            FunctionParameter::Named(n) => n.segment(),
            FunctionParameter::Variadic(_, s) => s.clone(),
            FunctionParameter::Slf(s) => s.clone(),
        }
    }
}
