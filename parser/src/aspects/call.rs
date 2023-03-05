use lexer::token::TokenType;
use crate::aspects::redirection::RedirectionAspect;
use crate::ast::callable::Call;
use crate::ast::Expr;
use crate::moves::{eox, spaces, MoveOperations, like, word_sep, repeat};
use crate::parser::{ParseResult, Parser};

/// A parse aspect for command and function calls
pub trait CallAspect<'a> {
    /// Attempts to parse the next call expression
    fn call(&mut self) -> ParseResult<Expr<'a>>;

    /// Continues to parse a call expression from a known command name expression
    fn call_arguments(&mut self, command: Expr<'a>) -> ParseResult<Expr<'a>>;
}

impl<'a> CallAspect<'a> for Parser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let cmd = self.next_value()?;
        self.call_arguments(cmd)
    }

    fn call_arguments(&mut self, command: Expr<'a>) -> ParseResult<Expr<'a>> {
        let mut arguments = vec![command];
        // Continue reading arguments until we reach the end of the input or a closing punctuation
        while !self.cursor.is_at_end()
            && self
                .cursor
                .lookahead(
                    spaces().then(
                        eox()
                            .or(like(TokenType::is_ponctuation))
                    ),
                )
            .is_none()
        {
            self.cursor.advance(repeat(word_sep()));

            if self.is_at_redirection_sign() {
                return self.redirectable(Expr::Call(Call { arguments }));
            }
            arguments.push(self.next_value()?);
        }
        self.cursor.advance(repeat(word_sep()));

        if self.is_at_redirection_sign() {
            return self.redirectable(Expr::Call(Call { arguments }));
        }
        Ok(Expr::Call(Call { arguments }))
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use lexer::lexer::lex;

    use crate::ast::callable::Call;
    use crate::ast::value::Literal;
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
    fn multiline_call() {
        let tokens = lex("g++ -std=c++20 \\\n-Wall \\\n-Wextra\\\n-Wpedantic");
        let parsed = parse(tokens).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![Expr::Call(Call {
                arguments: vec![
                    Expr::Literal("g++".into()),
                    Expr::Literal("-std=c++20".into()),
                    Expr::Literal("-Wall".into()),
                    Expr::Literal("-Wextra".into()),
                    Expr::Literal("-Wpedantic".into()),
                ],
            }),]
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
                        lexeme: "\\;",
                        parsed: ";".into(),
                    }),
                    Expr::Literal("echo".into()),
                    Expr::Literal("test".into()),
                ],
            }),]
        )
    }
}
