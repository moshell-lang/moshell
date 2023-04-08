use ast::value::Literal;
use ast::Expr;
use context::source::SourceSegment;

pub fn literal<'a>(source: &'a str, literal: &'_ str) -> Expr<'a> {
    literal_nth(source, literal, 0)
}

pub fn literal_nth<'a>(source: &'a str, literal: &'_ str, nth: usize) -> Expr<'a> {
    let segment = find_in_nth(source, literal, nth);
    // Remove quotes from the lexeme if present, start and end independently
    let mut parsed = if literal.starts_with('"') || literal.starts_with('\'') {
        &literal[1..]
    } else {
        literal
    };
    parsed = if literal.ends_with('"') || literal.ends_with('\'') {
        &parsed[..parsed.len() - 1]
    } else {
        parsed
    };

    Expr::Literal(Literal {
        parsed: parsed.into(),
        segment,
    })
}

pub fn find_in<'a>(source: &'a str, needle: &'a str) -> SourceSegment {
    let start = source.find(needle).expect("String not found.");
    start..start + needle.len()
}

///finds the nth match of needle in input source
pub fn find_in_nth<'a>(source: &'a str, needle: &'a str, nth: usize) -> SourceSegment {
    let start = source
        .match_indices(needle)
        .nth(nth)
        .expect("String not found.")
        .0;
    start..start + needle.len()
}

pub fn rfind_in<'a>(source: &'a str, needle: &'a str) -> SourceSegment {
    let start = source.rfind(needle).expect("String not found.");
    start..start + needle.len()
}

pub fn find_between<'a>(source: &'a str, start: &'a str, end: &'a str) -> SourceSegment {
    let start = source.find(start).expect("Start not found.");
    let end = source[start + 1..].find(end).expect("End not found.") + 1 + end.len();
    start..start + end
}

pub fn rfind_between<'a>(source: &'a str, start: &'a str, end: &'a str) -> SourceSegment {
    let end = source.rfind(end).expect("End not found.");
    let start = source[..end].rfind(start).expect("Start not found.");
    start..end + 1
}
