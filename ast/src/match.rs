use crate::value::{Literal, TemplateString};
use crate::variable::VarReference;
use crate::Expr;
use dbg_pls::DebugPls;

/// structure of a `match` expression.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Match<'a> {
    pub operand: Box<Expr<'a>>,
    pub arms: Vec<MatchArm<'a>>,
}

///the arm (a@ b | c if d => ..) of a match expression
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct MatchArm<'a> {
    //the extracted value name (x@ ..)
    pub val_name: Option<&'a str>,
    //the pattern (.. x | $y | "z" ..)
    pub patterns: Vec<MatchPattern<'a>>,
    //the arm's guard (.. if .. => ..)
    pub guard: Option<Expr<'a>>,
    //the body (.. => <body>)
    pub body: Expr<'a>,
}

///all different kinds of patterns available for a pattern expression
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum MatchPattern<'a> {
    //*, any
    Wildcard,

    //refer to wrapped structures documentation
    VarRef(VarReference<'a>),
    Literal(Literal<'a>),
    Template(TemplateString<'a>),
}
