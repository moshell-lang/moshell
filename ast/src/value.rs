use crate::Expr;
use dbg_pls::DebugPls;

/// A literal value that can be used directly.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Literal<'a> {
    pub lexeme: &'a str,
    pub parsed: LiteralValue,
}

/// A literal value that can be used directly.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum LiteralValue {
    String(String),
    Int(i64),
    Float(f64),
}

/// A group of expressions that can be interpolated into a string.
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

impl<'a> From<&'a str> for Literal<'a> {
    fn from(s: &'a str) -> Self {
        Self {
            lexeme: s,
            parsed: LiteralValue::from(s),
        }
    }
}
