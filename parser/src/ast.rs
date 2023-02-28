use crate::ast::group::{Block, Parenthesis, Subshell};
use crate::ast::callable::{Call, FunDeclaration, Pipeline, Redirected};
use crate::ast::literal::Literal;
use crate::ast::operation::BinaryOperation;
use crate::ast::substitution::Substitution;
use crate::ast::variable::{Assign, VarDeclaration, VarReference};

pub mod callable;
pub mod group;
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
    Pipeline(Pipeline<'a>),
    Redirected(Redirected<'a>),
    Substitution(Substitution<'a>),
    TemplateString(Vec<Expr<'a>>),

    //var / val handling expressions
    VarReference(VarReference<'a>),
    VarDeclaration(VarDeclaration<'a>),

    //Grouping expressions
    /// a parenthesis expression `( ... )` that evaluates a value
    Parenthesis(Parenthesis<'a>),
    /// a subshell expression `( ... )` that evaluates sub expressions in a new process
    Subshell(Subshell<'a>),
    /// a block expression `{ ... }`
    Block(Block<'a>),
}
