use crate::aspects::group::GroupAspect;
use crate::aspects::var_reference::VarReferenceAspect;
use crate::ast::substitution::{Substitution, SubstitutionKind};
use crate::ast::Expr;
use crate::moves::{of_type, of_types};
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType;

/// A parser for substitution expressions.
pub(crate) trait SubstitutionAspect<'a> {
    /// Parses a substitution expression, i.e. a variable reference or a command capture.
    fn substitution(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> SubstitutionAspect<'a> for Parser<'a> {
    fn substitution(&mut self) -> ParseResult<Expr<'a>> {
        let start_token = self.cursor.force(
            of_types(&[TokenType::At, TokenType::Dollar]),
            "Expected at or dollar sign.",
        )?;
        let next_token = self
            .cursor
            .lookahead(of_type(TokenType::RoundedLeftBracket));

        // Short pass for variable references
        if next_token.is_none() && start_token.token_type == TokenType::Dollar {
            return self.var_reference();
        }

        // Read the expression inside the parentheses as a new statement
        let expr = self.subshell()?;
        // self.cursor.force_with(
        //     space().then(of_type(TokenType::RoundedRightBracket)),
        //     "Expected closing bracket.",
        //     ParseErrorKind::Unpaired(self.cursor.relative_pos(&next_token.unwrap())),
        // )?;

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
    use crate::aspects::substitution::SubstitutionAspect;
    use crate::ast::callable::Call;
    use crate::ast::substitution::{Substitution, SubstitutionKind};
    use crate::ast::Expr;

    use crate::ast::group::{Block, Subshell};
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use crate::source::Source;
    use pretty_assertions::assert_eq;

    #[test]
    fn unterminated_substitution() {
        let content = "$(echo";
        let source = Source::unknown(content);
        let ast = Parser::new(source).substitution();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Unexpected end of expression".to_string(),
                position: content.len() - 1..content.len(),
                kind: ParseErrorKind::Unexpected
            })
        );
    }

    #[test]
    fn unpaired_parenthesis() {
        let content = "$(a @(b) $(c d\\))";
        let source = Source::unknown(content);
        let ast: ParseResult<_> = parse(source).into();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Unexpected end of expression".to_string(),
                position: content.len() - 1..content.len(),
                kind: ParseErrorKind::Unexpected
            })
        );
    }

    #[test]
    fn mix_blocks() {
        let source = Source::unknown("$({ls $(pwd)})");
        let ast = Parser::new(source).substitution().expect("Failed to parse");
        assert_eq!(
            ast,
            Expr::Substitution(Substitution {
                underlying: Subshell {
                    expressions: vec![Expr::Block(Block {
                        expressions: vec![Expr::Call(Call {
                            arguments: vec![
                                Expr::Literal("ls".into()),
                                Expr::Substitution(Substitution {
                                    underlying: Subshell {
                                        expressions: vec![Expr::Call(Call {
                                            arguments: vec![Expr::Literal("pwd".into())]
                                        })],
                                    },
                                    kind: SubstitutionKind::Capture,
                                }),
                            ],
                        })]
                    })],
                },
                kind: SubstitutionKind::Capture,
            })
        );
    }

    #[test]
    fn unexpected_closing_parenthesis() {
        let content = "some stuff)";
        let source = Source::unknown(content);
        let ast: ParseResult<_> = parse(source).into();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "expected end of expression or file, found ')'".to_string(),
                position: content.find(')').map(|p| (p..p + 1)).unwrap(),
                kind: ParseErrorKind::Unexpected
            })
        );
    }
}
