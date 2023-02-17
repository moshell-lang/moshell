use crate::ast::callable::{Call, FunDeclaration, Pipeline};
use crate::ast::literal::Literal;
use crate::ast::operation::BinaryOperation;
use crate::ast::substitution::Substitution;
use crate::ast::variable::{Assign, VarDeclaration, VarReference};

pub mod callable;
pub mod literal;
pub mod operation;
pub mod substitution;
pub mod variable;

/// A expression that can be evaluated.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Assign(Assign<'a>),
    Binary(BinaryOperation<'a>),
    Call(Call<'a>),
    FunDeclaration(FunDeclaration<'a>),
    Literal(Literal<'a>),
    //Grouping(Grouping<'a>),
    Pipeline(Pipeline<'a>),
    Substitution(Substitution<'a>),
    TemplateString(Vec<Expr<'a>>),
    VarReference(VarReference<'a>),
    VarDeclaration(VarDeclaration<'a>),
}

/*
/// A boxed expression that helps with precedence.
#[derive(Debug, Clone, PartialEq)]
pub struct Grouping<'a> {
    pub expr: Box<Expr<'a>>,
}
*/
