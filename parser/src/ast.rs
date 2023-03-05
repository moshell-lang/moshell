use crate::ast::callable::{Call, FunDeclaration, Pipeline, Redirected};
use crate::ast::group::{Block, Parenthesis, Subshell};
use crate::ast::literal::Literal;
use crate::ast::operation::BinaryOperation;
use crate::ast::r#use::Use;
use crate::ast::structure::Construct;
use crate::ast::substitution::Substitution;
use crate::ast::test::{Not, Test};
use crate::ast::variable::{Assign, VarDeclaration, VarReference};
use crate::ast::control_flow::If;

pub mod callable;
pub mod group;
pub mod literal;
pub mod operation;
pub mod structure;
pub mod substitution;
pub mod variable;
pub mod test;
pub mod r#use;
pub mod control_flow;

/// A expression that can be evaluated.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Assign(Assign<'a>),
    Binary(BinaryOperation<'a>),
    FunDeclaration(FunDeclaration<'a>),
    Literal(Literal<'a>),

    Call(Call<'a>),
    Pipeline(Pipeline<'a>),
    Redirected(Redirected<'a>),
    Construct(Construct<'a>),

    Substitution(Substitution<'a>),
    TemplateString(Vec<Expr<'a>>),

    Use(Use<'a>),

    Test(Test<'a>),
    Not(Not<'a>),

    If(If<'a>),

    //var / val handling expressions
    VarReference(VarReference<'a>),
    VarDeclaration(VarDeclaration<'a>),

    //Grouping expressions
    /// a parenthesis expression `( ... )` that contains one value expression
    Parenthesis(Parenthesis<'a>),
    /// a subshell expression `( ... )` that contains several expressions
    Subshell(Subshell<'a>),
    /// a block expression `{ ... }` that contains several expressions
    Block(Block<'a>),
}
