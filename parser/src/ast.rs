use crate::ast::callable::{Call, FunDeclaration};
use crate::ast::literal::Literal;
use crate::ast::operation::BinaryOperation;
use crate::ast::group::{Block, Parenthesis};
use crate::ast::substitution::Substitution;
use crate::ast::variable::{Assign, VarDeclaration, VarReference};

pub mod callable;
pub mod literal;
pub mod operation;
pub mod group;
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
    Substitution(Substitution<'a>),
    TemplateString(Vec<Expr<'a>>),

    //var / val handling expressions
    VarReference(VarReference<'a>),
    VarDeclaration(VarDeclaration<'a>),

    //Grouping expressions
    /// a parenthesis expression `( ... )`
    Parenthesis(Parenthesis<'a>),
    /// a block expression `{ ... }`
    Block(Block<'a>),
}
