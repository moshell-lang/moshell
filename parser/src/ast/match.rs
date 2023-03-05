use crate::ast::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct Match<'a> {
    pub operand: Box<Expr<'a>>,
    pub arms: Vec<MatchArm<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm<'a> {
    pub var_name: Option<&'a str>,
    pub pattern: Option<MatchPattern<'a>>,
    pub guard: Option<Expr<'a>>,
    pub body: Box<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchPattern<'a> {
    pub kind: MatchPatternKind<'a>,
    pub next: Option<Box<MatchPattern<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchPatternKind<'a> {
    VarRef(&'a str),
    Literal(&'a str),
}