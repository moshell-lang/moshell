use crate::ast::callable::{Call, FunDeclaration, Pipeline, Redirected};
use crate::ast::group::{Block, Parenthesis, Subshell};
use crate::ast::literal::Literal;
use crate::ast::operation::BinaryOperation;
use crate::ast::substitution::Substitution;
use crate::ast::test::{Not, Test};
use crate::ast::variable::{Assign, VarDeclaration, VarReference};
use crate::ast::flow_control::If;

pub mod callable;
pub mod group;
pub mod literal;
pub mod operation;
pub mod substitution;
pub mod variable;
pub mod test;
pub mod flow_control;

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
