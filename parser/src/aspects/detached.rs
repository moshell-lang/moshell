use ast::call::Detached;
use ast::Expr;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType::Ampersand;

use crate::err::ParseErrorKind;
use crate::moves::{of_type, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};

///parses a detached expression (<expr> &)
pub trait DetachedAspect<'a> {
    ///returns a Detached expression containing underlying,
    /// or directly returns underlying of no trailing '&' was found
    fn parse_detached(&mut self, underlying: Expr<'a>) -> ParseResult<Expr<'a>>;
}

impl<'a> DetachedAspect<'a> for Parser<'a> {
    fn parse_detached(&mut self, underlying: Expr<'a>) -> ParseResult<Expr<'a>> {
        let ampersand = spaces().then(of_type(Ampersand));
        //there is a trailing '&'
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
    use context::source::{Source, SourceSegmentHolder};
    use context::str_find::{find_between, find_in};

    use crate::err::ParseError;
    use crate::err::ParseErrorKind::Unexpected;
    use crate::parse;
    use crate::parser::ParseResult;
    use crate::source::literal;

    #[test]
    fn twice_derived() {
        let content = "date & &";
        let res: ParseResult<_> = parse(Source::unknown(content)).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "'&' not allowed here".to_string(),
                position: content.len() - 1..content.len(),
                kind: Unexpected,
            })
        )
    }

    #[test]
    fn twice_derived_workaround() {
        let source = Source::unknown("{date &}&");
        let res = parse(source).expect("Failed to parse");
        assert_eq!(
            res,
            vec![Expr::Detached(Detached {
                underlying: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Detached(Detached {
                        underlying: Box::new(Expr::Call(Call {
                            arguments: vec![literal(source.source, "date")],
                        })),
                        segment: find_in(source.source, "date &")
                    })],
                    segment: find_between(source.source, "{", "}")
                })),
                segment: source.segment()
            })]
        )
    }
}
