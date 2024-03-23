use ast::call::Detached;
use ast::Expr;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType::Ampersand;

use crate::err::ParseErrorKind;
use crate::moves::{of_type, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    /// Parses a detached expression containing an inner expression.
    ///
    /// If no detached expression is found, the inner expression is returned as is.
    pub(crate) fn parse_detached(&mut self, underlying: Expr) -> ParseResult<Expr> {
        let ampersand = spaces().then(of_type(Ampersand));
        // there is a trailing '&'
        if let Some(first) = self.cursor.advance(ampersand) {
            if let Some(another) = self.cursor.advance(ampersand) {
                return self.expected_with(
                    "'&' not allowed here",
                    another.span,
                    ParseErrorKind::Unexpected,
                );
            }
            let underlying = Box::new(underlying);
            let segment = underlying.segment().start..first.span.end;
            return Ok(Expr::Detached(Detached {
                underlying,
                segment,
            }));
        }
        Ok(underlying)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::{Call, Detached};
    use ast::group::Block;
    use ast::Expr;
    use context::source::SourceSegmentHolder;
    use context::str_find::{find_between, find_in};

    use crate::err::ParseError;
    use crate::err::ParseErrorKind::Unexpected;
    use crate::parse;
    use crate::parser::ParseResult;
    use crate::source::literal;

    #[test]
    fn twice_derived() {
        let source = "date & &";
        let res: ParseResult<_> = parse(source).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "'&' not allowed here".to_string(),
                position: source.len() - 1..source.len(),
                kind: Unexpected,
            })
        )
    }

    #[test]
    fn twice_derived_workaround() {
        let source = "{date &}&";
        let res = parse(source).expect("Failed to parse");
        assert_eq!(
            res,
            vec![Expr::Detached(Detached {
                underlying: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Detached(Detached {
                        underlying: Box::new(Expr::Call(Call {
                            arguments: vec![literal(source, "date")],
                        })),
                        segment: find_in(source, "date &")
                    })],
                    segment: find_between(source, "{", "}")
                })),
                segment: source.segment()
            })]
        )
    }
}
