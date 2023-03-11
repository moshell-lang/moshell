use crate::ast::variable::VarReference;
use crate::ast::Expr;
use dbg_pls::DebugPls;

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Iterable<'a> {
    Range(NumericRange<'a>),
    Files(FilePattern<'a>),
    Var(VarReference<'a>),
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct NumericRange<'a> {
    pub start: Expr<'a>,
    pub end: Expr<'a>,
    pub step: Option<Expr<'a>>,
    pub upper_inclusive: bool,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct FilePattern<'a> {
    pub lexeme: &'a str,
    pub pattern: String,
}
