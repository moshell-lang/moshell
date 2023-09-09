use dbg_pls::DebugPls;

use src_macros::segment_holder;

use crate::r#type::{GenericType, Type};
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
    pub type_parameters: Vec<GenericType<'a>>,
    pub parameters: Vec<FunctionParameter<'a>>,
    pub return_type: Option<Type<'a>>,
    pub body: Box<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum FunctionParameter<'a> {
    Named(TypedVariable<'a>),
    ///argument is the type of the variable (if any).
    Variadic(Option<Type<'a>>),
}
