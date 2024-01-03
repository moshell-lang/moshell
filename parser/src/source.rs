use ast::value::Literal;
use ast::variable::Identifier;
use ast::Expr;
use context::str_find;

pub fn identifier<'a>(source: &'a str, identifier: &'_ str) -> Identifier<'a> {
    identifier_nth(source, identifier, 0)
}

pub fn identifier_nth<'a>(source: &'a str, identifier: &'_ str, nth: usize) -> Identifier<'a> {
    let segment = str_find::find_in_nth(source, identifier, nth);
    Identifier::extract(source, segment)
}

pub fn literal<'a>(source: &'a str, literal: &'_ str) -> Expr<'a> {
    literal_nth(source, literal, 0)
}

pub fn literal_nth<'a>(source: &'a str, literal: &'_ str, nth: usize) -> Expr<'a> {
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
