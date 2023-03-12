use lexer::token::TokenType::Ampersand;

use crate::diagnostic::{ParseDiagnosisReporter, ParseErrorKind, ParseResult};
use crate::moves::{of_type, word_seps, MoveOperations};
use crate::parser::{Parser};
use ast::callable::Detached;
use ast::Expr;

///parses a detached expression (<expr> &)
pub trait DetachedAspect<'a> {
    ///returns a Detached expression containing underlying,
    /// or directly returns underlying of no trailing '&' was found
    fn parse_detached(&mut self, underlying: Expr<'a>) -> ParseResult<Expr<'a>>;
}

impl<'a, R: ParseDiagnosisReporter> DetachedAspect<'a> for Parser<'a, R> {
    fn parse_detached(&mut self, underlying: Expr<'a>) -> ParseResult<Expr<'a>> {
        let ampersand = word_seps().then(of_type(Ampersand));
        //there is a trailing '&'
        if self.cursor.advance(ampersand).is_some() {
            if let Some(another) = self.cursor.advance(ampersand) {
                return self.expected_with(
                    "'&' not allowed here",
                    another,
                    ParseErrorKind::Unexpected,
                );
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

    use ast::callable::{Call, Detached};
    use ast::group::Block;
    use ast::Expr;
    use ast::Expr::Literal;
    use context::source::Source;

    use crate::diagnostic::{ParseError, ParseResult};
    use crate::diagnostic::ParseErrorKind::Unexpected;
    use crate::parse;

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
    fn test_twice_derived_workaround() {
        let res = parse(Source::unknown("{date &}&")).expect("Failed to parse");
        assert_eq!(
            res,
            vec![Expr::Detached(Detached {
                underlying: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Detached(Detached {
                        underlying: Box::new(Expr::Call(Call {
                            arguments: vec![Literal("date".into())],
                            tparams: vec![],
                        }))
                    })]
                }))
            })]
        )
    }
}
