use crate::ast::Expr;
use crate::ast::value::{Literal, TemplateString};
use crate::ast::variable::VarReference;

#[derive(Debug, Clone, PartialEq)]
pub struct Match<'a> {
    pub operand: Box<Expr<'a>>,
    pub arms: Vec<MatchArm<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm<'a> {
    pub val_name: Option<&'a str>,
    pub patterns: Vec<MatchPattern<'a>>,
    pub guard: Option<Expr<'a>>,
    pub body: Box<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchPattern<'a> {
    VarRef(VarReference<'a>),
    Literal(Literal<'a>),
    Template(TemplateString<'a>),
    Wildcard,
}