use dbg_pls::DebugPls;
use crate::variable::{TypedVariable, VarDeclaration};
use crate::Expr;
use crate::r#type::Type;
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct FunctionDeclaration<'a> {
    pub name: &'a str,
    pub type_parameters: Vec<Type<'a>>,
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
