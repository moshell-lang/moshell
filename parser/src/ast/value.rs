use crate::ast::Expr;

/// A literal value that can be used directly.
#[derive(Debug, Clone, PartialEq)]
pub struct Literal<'a> {
    pub lexeme: &'a str,
    pub parsed: LiteralValue,
}

/// A literal value that can be used directly.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    String(String),
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
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
