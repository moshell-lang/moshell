use lexer::token::TokenType::Ampersand;

use crate::ast::callable::Detached;
use crate::ast::Expr;
use crate::err::ParseErrorKind;
use crate::moves::{of_type, word_seps, MoveOperations};
use crate::parser::{ParseResult, Parser};

///parses a detached expression (<expr> &)
pub trait DetachedAspect<'a> {
    ///returns a Detached expression containing underlying,
    /// or directly returns underlying of no trailing '&' was found
    fn parse_detached(&mut self, underlying: Expr<'a>) -> ParseResult<Expr<'a>>;
}

impl<'a> DetachedAspect<'a> for Parser<'a> {
    fn parse_detached(&mut self, underlying: Expr<'a>) -> ParseResult<Expr<'a>> {
        let ampersand = word_seps().then(of_type(Ampersand));
        //there is a trailing '&'
        if self.cursor.advance(ampersand).is_some() {
            if self.cursor.advance(ampersand).is_some() {
                //if there's another
                return self.expected("'&' not allowed here", ParseErrorKind::Unexpected);
            }
            return Ok(Expr::Detached(Detached {
                underlying: Box::new(underlying),
            }));
        }
        Ok(underlying)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use context::source::Source;
    use crate::ast::callable::{Call, Detached};
    use crate::ast::Expr;
    use crate::ast::Expr::Literal;
    use crate::ast::group::Block;

    use crate::err::ParseError;
    use crate::err::ParseErrorKind::Unexpected;
    use crate::parse;

    #[test]
    fn test_twice_derived() {
        let res = parse(Source::unknown("date & &")).errors;
        assert_eq!(
            res,
            vec![ParseError {
                message: "'&' not allowed here".to_string(),
                position: 8..8,
                kind: Unexpected,
            }]
        )
    }

    #[test]
    fn test_twice_derived_workaround() {
        let res = parse(Source::unknown("{date &}&")).unwrap();
        assert_eq!(
            res,
            vec![
                Expr::Detached(Detached {
                    underlying: Box::new(Expr::Block(Block {
                        expressions: vec![Expr::Detached(Detached {
                            underlying: Box::new(Expr::Call(Call {
                                arguments: vec![Literal("date".into())]
                            }))
                        })]
                    }))
                })
            ]
        )
    }
}
