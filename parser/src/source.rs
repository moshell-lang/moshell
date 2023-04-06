use crate::aspects::literal::literal_expr;
use ast::value::Literal;
use ast::Expr;
use context::source::SourceSegment;

pub fn literal<'a>(source: &'a str, literal: &'_ str) -> Expr<'a> {
    Expr::Literal(literal_expr(source, literal))
}

pub fn literal_nth<'a>(source: &'a str, literal: &'_ str, nth: usize) -> Expr<'a> {
    let segment = find_in_nth(source, literal, nth);
    Expr::Literal(Literal {
        parsed: match literal.chars().next().unwrap() {
            '"' | '\'' => literal[1..literal.len() - 1].into(),
            _ => literal.into(),
        },
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
