use lexer::token::TokenType;
use lexer::token::TokenType::*;

use crate::ast::variable::VarReference;
use crate::ast::Expr;
use crate::moves::{like, of_type};
use crate::parser::{ParseResult, Parser};

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
        let name = self
            .cursor
            .force(like(TokenType::is_valid_var_ref_name), "Expected valid variable name.")?.value;

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
    use crate::parse;
    use crate::parser::ParseError;

    #[test]
    fn test_simple_ref() {
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
    fn test_wrapped_ref() {
        let tokens = lex("${VAR}IABLE");
        let ast = parse(tokens).expect("failed to parse");
        assert_eq!(
            ast,
            vec![
                Expr::TemplateString(vec![
                    Expr::VarReference(VarReference {
                        name: "VAR"
                    }),
                    Expr::Literal("IABLE".into())
                ])
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
                Expr::TemplateString(vec![
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
                ])
            ]
        )
    }

}
