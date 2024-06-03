use ast::variable::{VarName, VarReference};
use ast::Expr;
use lexer::token::TokenType;

use crate::moves::{any, blanks, like, lookahead, of_type, MoveOperations};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    /// Parses a variable reference.
    pub(crate) fn var_reference(&mut self) -> ParseResult<Expr> {
        let start = self
            .cursor
            .advance(blanks().then(lookahead(any())))
            .unwrap();
        let bracket = self.cursor.advance(of_type(TokenType::CurlyLeftBracket));

        let name = self
            .cursor
            .force(
                like(TokenType::is_valid_var_ref_name),
                "Expected variable name.",
            )
            .map_err(|mut err| {
                err.position = self.cursor.peek().span;
                if bracket.is_some() {
                    self.repos_to_top_delimiter();
                }
                err
            })?;

        let mut segment = start.span.start..name.span.end;
        segment.start -= 1;

        let expr = Expr::VarReference(VarReference {
            name: if name.token_type == TokenType::Slf {
                VarName::Slf
            } else {
                VarName::User(name.text(self.source).into())
            },
            segment,
        });

        let mut expr = self.expand_member_chain(expr).map_err(|err| {
            if bracket.is_some() {
                self.repos_delimiter_due_to(&err);
            }
            err
        })?;

        if let Some(bracket) = bracket {
            self.expect_delimiter(bracket, TokenType::CurlyRightBracket)?;

            if let Expr::VarReference(ref mut var) = expr {
                var.segment.end += 1;
            }
        }
        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::MethodCall;
    use ast::value::{Literal, TemplateString};
    use ast::variable::{VarName, VarReference};
    use ast::Expr;
    use context::source::SourceSegmentHolder;
    use context::str_find::find_in;

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use crate::source::literal;

    #[test]
    fn simple_ref() {
        let source = "$VARIABLE";
        let ast = Parser::new(source).substitution().expect("failed to parse");
        assert_eq!(
            ast,
            Expr::VarReference(VarReference {
                name: VarName::User("VARIABLE".into()),
                segment: source.segment()
            })
        );
    }

    #[test]
    fn dollar_is_literal() {
        let source = "$";
        let ast = parse(source).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::Literal(Literal {
                parsed: "$".into(),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn special_refs() {
        let source = "$@;$^;$!;$$";
        let ast = parse(source).expect("failed to parse");
        assert_eq!(
            ast,
            vec![
                Expr::VarReference(VarReference {
                    name: VarName::User("@".into()),
                    segment: find_in(source, "$@"),
                }),
                Expr::VarReference(VarReference {
                    name: VarName::User("^".into()),
                    segment: find_in(source, "$^"),
                }),
                Expr::VarReference(VarReference {
                    name: VarName::User("!".into()),
                    segment: find_in(source, "$!"),
                }),
                Expr::VarReference(VarReference {
                    name: VarName::User("$".into()),
                    segment: find_in(source, "$$"),
                }),
            ]
        )
    }

    #[test]
    fn wrapped_ref() {
        let source = "${VAR}IABLE";
        let ast = Parser::new(source)
            .parse_specific(Parser::call_argument)
            .expect("failed to parse");
        assert_eq!(
            ast,
            Expr::TemplateString(TemplateString {
                parts: vec![
                    Expr::VarReference(VarReference {
                        name: VarName::User("VAR".into()),
                        segment: find_in(source, "${VAR}")
                    }),
                    Expr::Literal(Literal {
                        parsed: "IABLE".into(),
                        segment: find_in(source, "IABLE")
                    }),
                ],
                segment: source.segment()
            })
        )
    }

    #[test]
    fn ref_in_ref() {
        let source = "${V${A}R}";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Expected '}' delimiter.".to_string(),
                position: 3..4,
                kind: ParseErrorKind::Unpaired(source.find('{').map(|p| p..p + 1).unwrap()),
            })
        )
    }

    #[test]
    fn multiple_wrapped_ref() {
        let source = "${VAR}IABLE${LONG}${VERY_LONG}";
        let ast = Parser::new(source)
            .parse_specific(Parser::call_argument)
            .expect("failed to parse");
        assert_eq!(
            ast,
            Expr::TemplateString(TemplateString {
                parts: vec![
                    Expr::VarReference(VarReference {
                        name: VarName::User("VAR".into()),
                        segment: find_in(source, "${VAR}")
                    }),
                    literal(source, "IABLE"),
                    Expr::VarReference(VarReference {
                        name: VarName::User("LONG".into()),
                        segment: find_in(source, "${LONG}")
                    }),
                    Expr::VarReference(VarReference {
                        name: VarName::User("VERY_LONG".into()),
                        segment: find_in(source, "${VERY_LONG}")
                    }),
                ],
                segment: source.segment()
            })
        )
    }

    #[test]
    fn call_ref() {
        let source = "$callable('a', 'b', 'c')";
        let ast = parse(source).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::MethodCall(MethodCall {
                source: Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("callable".into()),
                    segment: find_in(source, "$callable")
                })),
                name: None,
                arguments: vec![
                    literal(source, "'a'"),
                    literal(source, "'b'"),
                    literal(source, "'c'"),
                ],
                type_parameters: vec![],
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn mismatched_delimiter() {
        let source = "${VAR)}";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Mismatched closing delimiter.".to_owned(),
                position: source.find(')').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unpaired(source.find('{').map(|p| p..p + 1).unwrap()),
            })
        )
    }
}
