use lexer::token::Token;

/// A literal value that can be used directly.
#[derive(Debug, Clone, PartialEq)]
pub struct Literal<'a> {
    pub token: Token<'a>,
    pub parsed: LiteralValue,
}

/// A literal value that can be used directly.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    String(String),
    Int(i64),
    Float(f64),
}
