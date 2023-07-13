use dbg_pls::DebugPls;

use src_macros::segment_holder;

use crate::Expr;

/// A literal value that can be used directly.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Literal {
    pub parsed: LiteralValue,
}

/// A literal value that can be used directly.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum LiteralValue {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

/// A group of expressions that can be interpolated into a string.
#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct TemplateString<'a> {
    pub parts: Vec<Expr<'a>>,
}

impl From<&str> for LiteralValue {
    fn from(s: &str) -> Self {
        Self::String(s.to_string())
    }
}

impl From<i64> for LiteralValue {
    fn from(s: i64) -> Self {
        Self::Int(s)
    }
}

impl From<f64> for LiteralValue {
    fn from(s: f64) -> Self {
        Self::Float(s)
    }
}

impl From<bool> for LiteralValue {
    fn from(s: bool) -> Self {
        Self::Bool(s)
    }
}
