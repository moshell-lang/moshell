use crate::ast::callable::{Call, FunDeclaration};
use crate::ast::literal::Literal;
use crate::ast::operation::BinaryOperation;
use crate::ast::substitution::Substitution;
use crate::ast::variable::{Assign, VarDeclaration, VarReference};

mod variable;
mod operation;
mod literal;
mod substitution;
mod callable;

/// A expression that can be evaluated.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Assign(Assign<'a>),
    Binary(BinaryOperation<'a>),
    Call(Call<'a>),
    FunDeclaration(FunDeclaration<'a>),
    Literal(Literal<'a>),
    Grouping(Grouping<'a>),
    Substitution(Substitution<'a>),
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

