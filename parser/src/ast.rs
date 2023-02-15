use crate::ast::callable::{Call, FunDeclaration};
use crate::ast::literal::Literal;
use crate::ast::operation::BinaryOperation;
use crate::ast::statement::Block;
use crate::ast::substitution::Substitution;
use crate::ast::variable::{Assign, VarDeclaration, VarReference};

pub mod variable;
pub mod operation;
pub mod literal;
pub mod substitution;
pub mod callable;
pub mod statement;

/// A expression that can be evaluated.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Assign(Assign<'a>),
    Binary(BinaryOperation<'a>),
    Call(Call<'a>),
    FunDeclaration(FunDeclaration<'a>),
    Literal(Literal<'a>),
    //Grouping(Grouping<'a>),
    Substitution(Substitution<'a>),
    VarReference(VarReference<'a>),
    VarDeclaration(VarDeclaration<'a>),
    Block(Block<'a>)
}

