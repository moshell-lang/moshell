#![allow(dead_code)]
#![deny(warnings)]

use dbg_pls::DebugPls;

use crate::call::{Call, Detached, MethodCall, Pipeline, ProgrammaticCall, Redirected};
use crate::control_flow::{For, If, Loop, While};
use crate::function::{FunctionDeclaration, Return};
use crate::group::{Block, Parenthesis, Subshell};
use crate::operation::BinaryOperation;
use crate::r#match::Match;
use crate::r#use::Use;
use crate::range::Iterable;
use crate::substitution::Substitution;
use crate::test::{Not, Test};
use crate::value::{Literal, TemplateString};
use crate::variable::{Assign, VarDeclaration, VarReference};

pub mod call;
pub mod control_flow;
pub mod function;
pub mod group;
pub mod r#match;
pub mod operation;
pub mod range;
pub mod substitution;
pub mod test;
pub mod r#type;
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
    ProgrammaticCall(ProgrammaticCall<'a>),
    MethodCall(MethodCall<'a>),
    Pipeline(Pipeline<'a>),
    Redirected(Redirected<'a>),
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
    Return(Return<'a>),

    //var / val handling expressions
    VarReference(VarReference<'a>),
    VarDeclaration(VarDeclaration<'a>),
    Range(Iterable<'a>),

    FunctionDeclaration(FunctionDeclaration<'a>),

    //Grouping expressions
    /// a parenthesis expression `( ... )` that contains one value expression
    Parenthesis(Parenthesis<'a>),
    /// a subshell expression `( ... )` that contains several expressions
    Subshell(Subshell<'a>),
    /// a block expression `{ ... }` that contains several expressions
    Block(Block<'a>),
}
