use crate::aspects::redirection_parser::RedirectionParser;
use crate::ast::callable::Call;
use crate::ast::Expr;
use crate::moves::{eox, of_types, predicate, space, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType::{And, Or};

/// A parse aspect for command and function calls
pub trait CallParser<'a> {
    /// Attempts to parse the next call expression
    fn call(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> CallParser<'a> for Parser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let mut arguments = vec![self.next_value()?];
        // Continue reading arguments until we reach the end of the input or a closing ponctuation
        while !self.cursor.is_at_end()
            && self
                .cursor
                .lookahead(
                    spaces().then(
                        eox()
                            .or(predicate(|t| t.token_type.is_closing_ponctuation()))
                            .or(of_types(&[And, Or])),
                    ),
                )
                .is_none()
        {
            self.cursor.advance(space());
            if self.is_at_redirection_sign() {
                return self.redirectable(Expr::Call(Call { arguments }));
            }
            arguments.push(self.next_value()?);
        }
        Ok(Expr::Call(Call { arguments }))
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use lexer::lexer::lex;

    use crate::ast::callable::Call;
    use crate::ast::literal::Literal;
    use crate::ast::Expr;
    use crate::parse;
    use crate::parser::{ParseError, Parser};

    #[test]
    fn wrong_group_end() {
        let tokens = lex("ls )");
        assert_eq!(
            Parser::new(tokens).parse_next(),
            Err(ParseError {
                message: "expected end of expression or file, found ')'".to_string()
            })
        );
    }

    #[test]
    fn multiple_calls() {
        let tokens = lex("grep -E regex; echo test");
        let parsed = parse(tokens).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![
                Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal("grep".into()),
                        Expr::Literal("-E".into()),
                        Expr::Literal("regex".into()),
                    ],
                }),
                Expr::Call(Call {
                    arguments: vec![Expr::Literal("echo".into()), Expr::Literal("test".into())],
                }),
            ]
        )
    }

    #[test]
    fn escaped_call() {
        let tokens = lex("grep -E regex \\; echo test");
        let parsed = parse(tokens).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![Expr::Call(Call {
                arguments: vec![
                    Expr::Literal("grep".into()),
                    Expr::Literal("-E".into()),
                    Expr::Literal("regex".into()),
                    Expr::Literal(Literal {
                        lexme: "\\;",
                        parsed: ";".into(),
                    }),
                    Expr::Literal("echo".into()),
                    Expr::Literal("test".into()),
                ],
            }),]
        )
    }
}
