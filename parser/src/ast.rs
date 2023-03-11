use crate::ast::callable::{Call, Detached, Pipeline, Redirected};
use crate::ast::control_flow::{For, If, Loop, While};
use crate::ast::group::{Block, Parenthesis, Subshell};
use crate::ast::operation::BinaryOperation;
use crate::ast::r#match::Match;
use crate::ast::r#use::Use;
use crate::ast::structure::Construct;
use crate::ast::substitution::Substitution;
use crate::ast::test::{Not, Test};
use crate::ast::value::{Literal, TemplateString};
use crate::ast::variable::{Assign, VarDeclaration, VarReference};
use dbg_pls::DebugPls;

pub mod callable;
pub mod control_flow;
pub mod group;
pub mod r#match;
pub mod operation;
pub mod range;
pub mod structure;
pub mod substitution;
pub mod test;
pub mod r#use;
pub mod value;
pub mod variable;

/// A expression that can be evaluated.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Expr<'a> {
    Assign(Assign<'a>),
    Binary(BinaryOperation<'a>),
    Literal(Literal<'a>),

    Match(Match<'a>),

    Call(Call<'a>),
    Pipeline(Pipeline<'a>),
    Redirected(Redirected<'a>),
    Construct(Construct<'a>),
    Detached(Detached<'a>),

    Substitution(Substitution<'a>),
    TemplateString(TemplateString<'a>),

    Use(Use<'a>),

    Test(Test<'a>),
    Not(Not<'a>),

    If(If<'a>),
    While(While<'a>),
    Loop(Loop<'a>),
    For(For<'a>),

    Continue,
    Break,

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
