#![allow(dead_code)]


use context::source::{SourceSegment, SourceSegmentHolder};
use dbg_pls::DebugPls;

use crate::call::{Call, Detached, MethodCall, Pipeline, ProgrammaticCall, Redirected};
use crate::control_flow::{For, If, Loop, While};
use crate::function::{FunctionDeclaration, Return};
use crate::group::{Block, Parenthesis, Subshell};
use crate::lambda::LambdaDef;
use crate::operation::BinaryOperation;
use crate::r#match::Match;
use crate::r#type::CastedExpr;
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
pub mod lambda;
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
    Literal(Literal),

    Match(Match<'a>),

    Call(Call<'a>),
    ProgrammaticCall(ProgrammaticCall<'a>),
    MethodCall(MethodCall<'a>),
    Pipeline(Pipeline<'a>),
    Redirected(Redirected<'a>),
    Detached(Detached<'a>),

    LambdaDef(LambdaDef<'a>),

    Substitution(Substitution<'a>),
    TemplateString(TemplateString<'a>),

    Use(Use<'a>),

    Casted(CastedExpr<'a>),

    Test(Test<'a>),
    Not(Not<'a>),

    If(If<'a>),
    While(While<'a>),
    Loop(Loop<'a>),
    For(For<'a>),

    Continue(SourceSegment),
    Break(SourceSegment),
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

impl SourceSegmentHolder for Expr<'_> {
    fn segment(&self) -> SourceSegment {
        match self {
            Expr::Assign(assign) => assign.segment(),
            Expr::Binary(binary) => binary.segment(),
            Expr::Literal(literal) => literal.segment.clone(),
            Expr::Match(m) => m.segment.clone(),
            Expr::Call(call) => call.segment(),
            Expr::ProgrammaticCall(call) => call.segment.clone(),
            Expr::MethodCall(method_call) => method_call.segment.clone(),
            Expr::Pipeline(pipeline) => pipeline.segment(),
            Expr::Redirected(redirected) => redirected.segment(),
            Expr::Detached(detached) => detached.segment.clone(),
            Expr::LambdaDef(lambda) => lambda.segment(),
            Expr::Substitution(substitution) => substitution.segment(),
            Expr::TemplateString(template_string) => template_string.segment(),
            Expr::Use(use_) => use_.segment.clone(),
            Expr::Casted(casted) => casted.segment(),
            Expr::Test(test) => test.segment.clone(),
            Expr::Not(not) => not.segment.clone(),
            Expr::If(if_) => if_.segment.clone(),
            Expr::While(while_) => while_.segment.clone(),
            Expr::Loop(loop_) => loop_.segment.clone(),
            Expr::For(for_) => for_.segment.clone(),
            Expr::Continue(source) => source.clone(),
            Expr::Break(source) => source.clone(),
            Expr::Return(return_) => return_.segment.clone(),
            Expr::VarReference(var_reference) => var_reference.segment.clone(),
            Expr::VarDeclaration(var_declaration) => var_declaration.segment.clone(),
            Expr::Range(range) => range.segment(),
            Expr::FunctionDeclaration(function_declaration) => function_declaration.segment.clone(),
            Expr::Parenthesis(parenthesis) => parenthesis.segment.clone(),
            Expr::Subshell(subshell) => subshell.segment.clone(),
            Expr::Block(block) => block.segment.clone(),
        }
    }
}
