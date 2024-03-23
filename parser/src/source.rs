use ast::value::Literal;
use ast::variable::Identifier;
use ast::Expr;
use context::str_find;

pub fn identifier(source: &str, identifier: &str) -> Identifier {
    identifier_nth(source, identifier, 0)
}

pub fn identifier_nth(source: &str, identifier: &str, nth: usize) -> Identifier {
    let segment = str_find::find_in_nth(source, identifier, nth);
    Identifier::extract(source, segment)
}

pub fn literal(source: &str, literal: &str) -> Expr {
    literal_nth(source, literal, 0)
}

pub fn literal_nth(source: &str, literal: &str, nth: usize) -> Expr {
    let segment = str_find::find_in_nth(source, literal, nth);
    // Remove quotes from the lexeme if present, start and end independently
    let mut parsed = literal;
    if parsed.starts_with('\'') || parsed.starts_with('"') {
        parsed = &parsed[1..];
    }
    if parsed.ends_with('\'') || parsed.ends_with('"') {
        parsed = &parsed[..parsed.len() - 1];
    }

    Expr::Literal(Literal {
        parsed: parsed.into(),
        segment,
    })
}
