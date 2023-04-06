use context::source::try_join_str;
use lexer::token::TokenType::*;

use crate::err::ParseErrorKind;
use crate::moves::{any, blanks, like, lookahead, of_type, repeat, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::variable::VarReference;
use ast::Expr;
use lexer::token::TokenType;

pub trait VarReferenceAspect<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> VarReferenceAspect<'a> for Parser<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>> {
        let start = self
            .cursor
            .advance(blanks().then(lookahead(any())))
            .unwrap();
        let bracket = self.cursor.advance(of_type(CurlyLeftBracket));

        let tokens = self
            .cursor
            .collect(
                of_type(Dollar) //only allow one occurrence of $
                    .or(repeat(like(TokenType::is_valid_var_ref_name))),
            )
            .leak();

        if tokens.is_empty() {
            return self.expected(
                "variable reference with empty name",
                ParseErrorKind::Unexpected,
            );
        }

        let first = tokens[0].value;
        let name = tokens.iter().skip(1).fold(first, |acc, t| {
            try_join_str(self.source.source, acc, t.value).unwrap()
        });

        let mut segment = self.cursor.relative_pos_ctx(start.value..name);
        segment.start -= 1;

        if let Some(bracket) = bracket {
            self.cursor.force_with(
                of_type(CurlyRightBracket),
                "Expected closing curly bracket.",
                ParseErrorKind::Unpaired(self.cursor.relative_pos(bracket)),
            )?;
            segment.end += 1;
        }

        Ok(Expr::VarReference(VarReference { name, segment }))
    }
}

#[cfg(test)]
mod tests {
    use crate::aspects::substitution::SubstitutionAspect;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use crate::source::{find_in, literal};
    use ast::value::{Literal, TemplateString};
    use ast::variable::VarReference;
    use ast::Expr;
    use context::source::{Source, SourceSegmentHolder};
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_ref() {
        let source = Source::unknown("$VARIABLE");
        let ast = Parser::new(source.clone())
            .substitution()
            .expect("failed to parse");
        assert_eq!(
            ast,
            Expr::VarReference(VarReference {
                name: "VARIABLE",
                segment: source.segment()
            })
        );
    }

    #[test]
    fn dollar_is_literal() {
        let source = Source::unknown("$");
        let ast = parse(source.clone()).expect("failed to parse");
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
        let source = Source::unknown("$@;$^;$!;$!!;$$");
        let ast = parse(source.clone()).expect("failed to parse");
        assert_eq!(
            ast,
            vec![
                Expr::VarReference(VarReference {
                    name: "@",
                    segment: find_in(&source.source, "$@"),
                }),
                Expr::VarReference(VarReference {
                    name: "^",
                    segment: find_in(&source.source, "$^"),
                }),
                Expr::VarReference(VarReference {
                    name: "!",
                    segment: find_in(&source.source, "$!"),
                }),
                Expr::VarReference(VarReference {
                    name: "!!",
                    segment: find_in(&source.source, "$!!"),
                }),
                Expr::VarReference(VarReference {
                    name: "$",
                    segment: find_in(&source.source, "$$"),
                }),
            ]
        )
    }

    #[test]
    fn wrapped_ref() {
        let source = Source::unknown("${VAR}IABLE");
        let ast = parse(source.clone()).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::TemplateString(TemplateString {
                parts: vec![
                    Expr::VarReference(VarReference {
                        name: "VAR",
                        segment: find_in(source.source, "${VAR}")
                    }),
                    Expr::Literal(Literal {
                        parsed: "IABLE".into(),
                        segment: find_in(source.source, "IABLE")
                    }),
                ]
            })]
        )
    }

    #[test]
    fn ref_in_ref() {
        let content = "${V${A}R}";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Expected closing curly bracket.".to_string(),
                position: 4..5,
                kind: ParseErrorKind::Unpaired(content.find('{').map(|p| p..p + 1).unwrap()),
            })
        )
    }

    #[test]
    fn multiple_wrapped_ref() {
        let source = Source::unknown("${VAR}IABLE${LONG}${VERY_LONG}");
        let ast = parse(source.clone()).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::TemplateString(TemplateString {
                parts: vec![
                    Expr::VarReference(VarReference {
                        name: "VAR",
                        segment: find_in(source.source, "${VAR}")
                    }),
                    literal(source.source, "IABLE"),
                    Expr::VarReference(VarReference {
                        name: "LONG",
                        segment: find_in(source.source, "${LONG}")
                    }),
                    Expr::VarReference(VarReference {
                        name: "VERY_LONG",
                        segment: find_in(source.source, "${VERY_LONG}")
                    }),
                ]
            })]
        )
    }
}
