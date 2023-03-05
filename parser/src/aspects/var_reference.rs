use lexer::token::TokenType::*;

use crate::ast::Expr;
use crate::ast::variable::VarReference;
use crate::moves::{like, MoveOperations, of_type, repeat};
use crate::parser::{ParseResult, Parser};
use crate::source::try_join_str;

pub trait VarReferenceAspect<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> VarReferenceAspect<'a> for Parser<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>> {
        let has_bracket = self
            .cursor
            .advance(of_type(CurlyLeftBracket))
            .is_some();

        let tokens = self.cursor.select(
            of_type(Dollar) //only allow one occurrence of $
                .or(repeat(like(|t| t != Dollar && t.is_valid_var_ref_name())))
        ).leak();


        if tokens.len() == 0 {
            return self.expected("variable reference with empty name")
        }

        let first = tokens[0].value;
        let name = tokens.into_iter()
            .skip(1)
            .fold(first, |acc, t| try_join_str(acc, t.value).unwrap());

        if has_bracket {
            self.cursor.force(
                of_type(CurlyRightBracket),
                "Expected closing curly bracket.",
            )?;
        }
        Ok(Expr::VarReference(VarReference { name }))
    }
}

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;

    use crate::ast::variable::VarReference;
    use crate::ast::Expr;
    use pretty_assertions::assert_eq;
    use crate::ast::value::TemplateString;
    use crate::parse;
    use crate::parser::ParseError;

    #[test]
    fn simple_ref() {
        let tokens = lex("$VARIABLE");
        let ast = parse(tokens).expect("failed to parse");
        assert_eq!(
            ast,
            vec![
                Expr::VarReference(VarReference {
                    name: "VARIABLE"
                })
            ]
        )
    }

    #[test]
    fn dollar_is_literal() {
        let tokens = lex("$");
        let ast = parse(tokens).expect("failed to parse");
        assert_eq!(
            ast,
            vec![
                Expr::Literal("$".into())
            ]
        )
    }

    #[test]
    fn special_refs() {
        let tokens = lex("$@;$^;$!;$!!;$$");
        let ast = parse(tokens).expect("failed to parse");
        assert_eq!(
            ast,
            vec![
                Expr::VarReference(VarReference {
                    name: "@"
                }),
                Expr::VarReference(VarReference {
                    name: "^"
                }),
                Expr::VarReference(VarReference {
                    name: "!"
                }),
                Expr::VarReference(VarReference {
                    name: "!!"
                }),
                Expr::VarReference(VarReference {
                    name: "$"
                }),
            ]
        )
    }

    #[test]
    fn wrapped_ref() {
        let tokens = lex("${VAR}IABLE");
        let ast = parse(tokens).expect("failed to parse");
        assert_eq!(
            ast,
            vec![
                Expr::TemplateString(TemplateString {
                    parts: vec![
                        Expr::VarReference(VarReference {
                            name: "VAR"
                        }),
                        Expr::Literal("IABLE".into()),
                    ]
                })
            ]
        )
    }

    #[test]
    fn test_ref_in_ref() {
        let tokens = lex("${V${A}R}");
        let ast = parse(tokens);
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Expected closing curly bracket.".to_string()
            })
        )
    }

    #[test]
    fn test_multiple_wrapped_ref() {
        let tokens = lex("${VAR}IABLE${LONG}${VERY_LONG}");
        let ast = parse(tokens).expect("failed to parse");
        assert_eq!(
            ast,
            vec![
                Expr::TemplateString(TemplateString {
                    parts: vec![
                        Expr::VarReference(VarReference {
                            name: "VAR"
                        }),
                        Expr::Literal("IABLE".into()),
                        Expr::VarReference(VarReference {
                            name: "LONG"
                        }),
                        Expr::VarReference(VarReference {
                            name: "VERY_LONG"
                        }),
                    ]
                })
            ]
        )
    }

}
