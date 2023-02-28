use crate::aspects::var_reference_parser::VarReferenceParser;
use crate::ast::substitution::{Substitution, SubstitutionKind};
use crate::ast::Expr;
use crate::moves::{of_type, of_types};
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType;
use crate::aspects::group_parser::GroupParser;

/// A parser for substitution expressions.
pub(crate) trait SubstitutionParser<'a> {
    /// Parses a substitution expression, i.e. a variable reference or a command capture.
    fn substitution(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> SubstitutionParser<'a> for Parser<'a> {
    fn substitution(&mut self) -> ParseResult<Expr<'a>> {
        let start_token = self.cursor.force(
            of_types(&[TokenType::At, TokenType::Dollar]),
            "Expected at or dollar sign.",
        )?;

        // Short pass for variable references
        if self
            .cursor
            .lookahead(of_type(TokenType::RoundedLeftBracket))
            .is_none()
            && start_token.token_type == TokenType::Dollar
        {
            return self.var_reference();
        }

        // Read the expression inside the parentheses as a new statement
        let expr = self.subshell()?;


        // Finally return the expression
        Ok(Expr::Substitution(Substitution {
            underlying: expr,
            kind: if start_token.token_type == TokenType::At {
                SubstitutionKind::Return
            } else {
                SubstitutionKind::Capture
            },
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::aspects::substitution_parser::SubstitutionParser;
    use crate::ast::callable::Call;
    use crate::ast::substitution::{Substitution, SubstitutionKind};
    use crate::ast::Expr;
    use crate::parse;
    use crate::parser::{ParseError, Parser};
    use lexer::lexer::lex;
    use pretty_assertions::assert_eq;
    use crate::ast::group::{Block, Subshell};

    #[test]
    fn unterminated_substitution() {
        let tokens = lex("$(echo");
        let ast = Parser::new(tokens).substitution();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Unexpected end of expression".to_string()
            })
        );
    }

    #[test]
    fn unpaired_parenthesis() {
        let tokens = lex("$(a @(b) $(c d\\))");
        let ast = parse(tokens);
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Unexpected end of expression".to_string()
            })
        );
    }

    #[test]
    fn mix_blocks() {
        let tokens = lex("$({ls $(pwd)})");
        let ast = Parser::new(tokens).substitution().expect("Failed to parse");
        assert_eq!(
            ast,
            Expr::Substitution(Substitution {
                underlying: Subshell {
                    expressions: vec![
                        Expr::Block(Block {
                            expressions: vec![
                                Expr::Call(Call {
                                    arguments: vec![
                                        Expr::Literal("ls".into()),
                                        Expr::Substitution(Substitution {
                                            underlying: Subshell {
                                                expressions: vec![
                                                    Expr::Call(Call {
                                                        arguments: vec![
                                                            Expr::Literal("pwd".into())
                                                        ]
                                                    })
                                                ],
                                            },
                                            kind: SubstitutionKind::Capture,
                                        }),
                                    ],
                                })
                            ]
                        }
                        )
                    ],
                },
                kind: SubstitutionKind::Capture,
            })
        );
    }

    #[test]
    fn unexpected_closing_parenthesis() {
        let tokens = lex("some stuff)");
        let ast = parse(tokens);
        assert_eq!(
            ast,
            Err(ParseError {
                message: "expected end of expression or file, found ')'".to_string()
            })
        );
    }
}
