use crate::ast::callable::{Call, Pipeline, Redir, RedirFd, RedirOp, Redirected};
use crate::ast::Expr;
use crate::moves::{eox, next, of_type, of_types, space, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType;

/// A parse aspect for command and function calls
pub trait CallParser<'a> {
    /// Attempts to parse the next call expression
    fn call(&mut self) -> ParseResult<Expr<'a>>;
    /// Attempts to parse the next pipeline expression
    fn pipeline(&mut self, first_call: Call<'a>) -> ParseResult<Expr<'a>>;
    /// Attempts to parse the next redirection
    fn redirection(&mut self) -> ParseResult<Redir<'a>>;
}

impl<'a> CallParser<'a> for Parser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let mut arguments = vec![self.expression()?];
        let mut redirections = vec![];
        // End Of Expression \!(; + \n)
        while !self.cursor.is_at_end() && self.cursor.advance(spaces().then(eox())).is_none() {
            self.cursor.advance(space());
            match self.cursor.peek().token_type {
                TokenType::Ampersand | TokenType::Less | TokenType::Greater => {
                    redirections.push(self.redirection()?);
                }
                TokenType::Pipe => {
                    return self.pipeline(Call { arguments });
                }
                // Detect redirections without a specific file descriptor
                _ if self
                    .cursor
                    .lookahead(next().then(of_types(&[TokenType::Less, TokenType::Greater])))
                    .is_some() =>
                {
                    redirections.push(self.redirection()?)
                }
                _ => arguments.push(self.expression()?),
            };
        }

        if redirections.is_empty() {
            Ok(Expr::Call(Call { arguments }))
        } else {
            Ok(Expr::Redirected(Redirected {
                expr: Box::new(Expr::Call(Call { arguments })),
                redirections,
            }))
        }
    }

    fn pipeline(&mut self, first_call: Call<'a>) -> ParseResult<Expr<'a>> {
        let mut commands = vec![Expr::Call(first_call)];
        // Continue as long as we have a pipe
        while self
            .cursor
            .advance(space().then(of_type(TokenType::Pipe)))
            .is_some()
        {
            match self.call()? {
                Expr::Pipeline(pipeline) => commands.extend(pipeline.commands),
                call => commands.push(call),
            }
        }
        Ok(Expr::Pipeline(Pipeline { commands }))
    }

    fn redirection(&mut self) -> ParseResult<Redir<'a>> {
        self.cursor.advance(space());
        let mut token = self.cursor.next()?;
        // Parse if present the redirected file descriptor
        let fd = match token.token_type {
            TokenType::Ampersand => {
                token = self.cursor.next()?;
                RedirFd::Wildcard
            }
            TokenType::IntLiteral => {
                let redir = RedirFd::Fd(
                    token
                        .value
                        .parse()
                        .map_err(|_| self.mk_parse_error("Invalid file descriptor."))?,
                );
                token = self.cursor.next()?;
                redir
            }
            _ => RedirFd::Default,
        };

        // Parse the redirection operator
        let mut operator = match token.token_type {
            TokenType::Less => {
                if self
                    .cursor
                    .advance(of_type(TokenType::Less).and_then(of_type(TokenType::Less)))
                    .is_some()
                {
                    RedirOp::String
                } else {
                    RedirOp::Read
                }
            }
            TokenType::Greater => match self.cursor.advance(of_type(TokenType::Greater)) {
                None => RedirOp::Write,
                Some(_) => RedirOp::Append,
            },
            _ => Err(self.mk_parse_error("Expected redirection operator."))?,
        };

        // Parse file descriptor duplication and update the operator
        if self.cursor.advance(of_type(TokenType::Ampersand)).is_some() {
            operator = match operator {
                RedirOp::Read => RedirOp::FdIn,
                RedirOp::Write => RedirOp::FdOut,
                _ => Err(self.mk_parse_error("Invalid redirection operator."))?,
            };
        }

        let operand = self.expression()?;
        Ok(Redir {
            fd,
            operator,
            operand,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::aspects::call_parser::CallParser;
    use crate::ast::callable::{Call, Redir, RedirFd, RedirOp, Redirected};
    use crate::ast::literal::Literal;
    use crate::ast::Expr;
    use crate::parse;
    use crate::parser::Parser;
    use lexer::lexer::lex;
    use lexer::token::{Token, TokenType};

    #[test]
    fn redirection() {
        let tokens = vec![
            Token::new(TokenType::Identifier, "ls"),
            Token::new(TokenType::Greater, ">"),
            Token::new(TokenType::Identifier, "/tmp/out"),
        ];
        let parsed = Parser::new(tokens).call().expect("Failed to parse");
        assert_eq!(
            parsed,
            Expr::Redirected(Redirected {
                expr: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "ls"),
                        parsed: "ls".into(),
                    })]
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::Write,
                    operand: Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "/tmp/out"),
                        parsed: "/tmp/out".into(),
                    }),
                }],
            })
        );
    }

    #[test]
    fn dupe_fd() {
        let tokens = vec![
            Token::new(TokenType::Identifier, "ls"),
            Token::new(TokenType::Greater, ">"),
            Token::new(TokenType::Ampersand, "&"),
            Token::new(TokenType::IntLiteral, "2"),
        ];
        let parsed = Parser::new(tokens).call().expect("Failed to parse");
        assert_eq!(
            parsed,
            Expr::Redirected(Redirected {
                expr: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "ls"),
                        parsed: "ls".into(),
                    })]
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::FdOut,
                    operand: Expr::Literal(Literal {
                        token: Token::new(TokenType::IntLiteral, "2"),
                        parsed: 2.into(),
                    }),
                }],
            })
        );
    }

    #[test]
    fn multiple_calls() {
        let tokens = lex("grep -E regex; echo test");
        let parsed = parse(tokens).expect("parsing error");
        assert_eq!(
            parsed,
            vec![
                Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "grep"),
                            parsed: "grep".into(),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "E"),
                            parsed: "-E".into(),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "regex"),
                            parsed: "regex".into(),
                        }),
                    ],
                }),
                Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "echo"),
                            parsed: "echo".into(),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "test"),
                            parsed: "test".into(),
                        }),
                    ],
                }),
            ]
        )
    }

    #[test]
    fn escaped_call() {
        let tokens = lex("grep -E regex \\; echo test");
        let parsed = parse(tokens).expect("parsing error");
        assert_eq!(
            parsed,
            vec![Expr::Call(Call {
                arguments: vec![
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "grep"),
                        parsed: "grep".into(),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "E"),
                        parsed: "-E".into(),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "regex"),
                        parsed: "regex".into(),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::BackSlash, "\\"),
                        parsed: ";".into(),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "echo"),
                        parsed: "echo".into(),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "test"),
                        parsed: "test".into(),
                    }),
                ],
            }),]
        )
    }
}
