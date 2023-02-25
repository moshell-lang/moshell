use lexer::token::TokenType;
use lexer::token::TokenType::{BackSlash, DoubleQuote, Quote};
use crate::aspects::call_parser::CallParser;
use crate::ast::callable::{Pipeline, Redir, Redirected, RedirFd, RedirOp};
use crate::ast::Expr;
use crate::moves::{Move, MoveOperations, next, of_type, of_types, space, spaces};
use crate::parser::{Parser, ParseResult};

pub(crate) trait RedirectionParser<'a> {
    /// Attempts to parse the next pipeline expression
    /// inputs an "end of call" statements to determine where the call can stop.
    fn pipeline(&mut self, first_call: Expr<'a>, eoc: impl Move + Copy) -> ParseResult<Expr<'a>>;
    /// Attempts to parse the next redirection
    fn redirection(&mut self) -> ParseResult<Redir<'a>>;
    /// Associates any potential redirections to a redirectable expression
    fn redirectable(&mut self, expr: Expr<'a>, eoc: impl Move + Copy) -> ParseResult<Expr<'a>>;

    ///return true if parser is currently on a redirection sign.
    fn is_at_redirection_sign(&self) -> bool;
}

impl<'a> RedirectionParser<'a> for Parser<'a> {
    fn pipeline(&mut self, first_call: Expr<'a>, eoc: impl Move + Copy) -> ParseResult<Expr<'a>> {
        let mut commands = vec![first_call];
        // Continue as long as we have a pipe
        while self
            .cursor
            .advance(space().then(of_type(TokenType::Pipe)))
            .is_some()
        {
            match self.call(eoc)? {
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

    fn redirectable(&mut self, expr: Expr<'a>, eoc: impl Move + Copy) -> ParseResult<Expr<'a>> {
        let mut redirections = vec![];
        self.cursor.advance(spaces());

        while self.cursor.lookahead(eoc).is_none() {
            match self.cursor.peek().token_type {
                TokenType::Ampersand | TokenType::Less | TokenType::Greater => {
                    redirections.push(self.redirection()?);
                }

                TokenType::Pipe => {
                    return self.pipeline(expr, eoc);
                }
                // Detect redirections without a specific file descriptor
                _ if self
                    .cursor
                    .lookahead(next().then(of_types(&[TokenType::Less, TokenType::Greater])))
                    .is_some() =>
                    {
                        redirections.push(self.redirection()?)
                    }
                _ => break,
            };
            self.cursor.advance(spaces());
        }

        if redirections.is_empty() {
            Ok(expr)
        } else {
            Ok(Expr::Redirected(Redirected {
                expr: Box::new(expr),
                redirections,
            }))
        }
    }

    fn is_at_redirection_sign(&self) -> bool {
        //handle escaped redirection signs (can be \\ for one-character signs or quoted signs)
        if self.cursor.lookahead(of_types(&[BackSlash, Quote, DoubleQuote])).is_some() {
            return false;
        }

        let pivot = self.cursor.peek().token_type;
        match pivot {
            TokenType::Ampersand | TokenType::Less | TokenType::Greater | TokenType::Pipe => true,
            //search for '>' or '<' in case of std-determined redirection sign (ex: 2>>)
            _ => self
                .cursor
                .lookahead(next().then(of_types(&[TokenType::Less, TokenType::Greater])))
                .is_some(),
        }
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use lexer::lexer::lex;
    use lexer::token::{Token, TokenType};
    use crate::aspects::call_parser::CallParser;
    use crate::ast::callable::{Call, Redir, Redirected, RedirFd, RedirOp};
    use crate::ast::Expr;
    use crate::ast::group::Block;
    use crate::ast::literal::{Literal, LiteralValue};
    use crate::moves::eox;
    use crate::parse;
    use crate::parser::Parser;

    #[test]
    fn expr_redirection() {
        let tokens = lex("{ls; cd} > /tmp/out");
        let parsed = parse(tokens).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![
                Expr::Redirected(Redirected {
                    expr: Box::new(Expr::Block(Block {
                        expressions: vec![
                            Expr::Call(Call {
                                arguments: vec![
                                    Expr::Literal(Literal {
                                        token: Token::new(TokenType::Identifier, "ls"),
                                        parsed: "ls".into(),
                                    })
                                ]
                            }),
                            Expr::Call(Call {
                                arguments: vec![
                                    Expr::Literal(Literal {
                                        token: Token::new(TokenType::Identifier, "cd"),
                                        parsed: "cd".into(),
                                    })
                                ]
                            }),
                        ]
                    })),
                    redirections: vec![
                        Redir {
                            operator: RedirOp::Write,
                            fd: RedirFd::Default,
                            operand: Expr::Literal(Literal {
                                token: Token::new(TokenType::Identifier, "out"),
                                parsed: LiteralValue::String("/tmp/out".to_string()),
                            }),
                        }
                    ],
                }),
            ]
        );
    }

    #[test]
    fn call_redirection() {
        let tokens = lex("ls> /tmp/out");
        let parsed = Parser::new(tokens).call(eox()).expect("Failed to parse");
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
                        token: Token::new(TokenType::Identifier, "out"),
                        parsed: "/tmp/out".into(),
                    }),
                }],
            })
        );
    }

    #[test]
    fn dupe_fd() {
        let tokens = lex("ls>&2");
        let parsed = Parser::new(tokens).call(eox()).expect("Failed to parse");
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
    fn escaped_call() {
        let tokens = lex("echo hello \\; \\| '2>' '>&2' \\<");
        let parsed = parse(tokens).expect("parsing error");
        assert_eq!(
            parsed,
            vec![Expr::Call(Call {
                arguments: vec![
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "echo"),
                        parsed: "echo".into(),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "hello"),
                        parsed: "hello".into(),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::BackSlash, "\\"),
                        parsed: ";".into(),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::BackSlash, "\\"),
                        parsed: "|".into(),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Quote, "'"),
                        parsed: "2>".into(),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Quote, "'"),
                        parsed: ">&2".into(),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::BackSlash, "\\"),
                        parsed: "<".into(),
                    }),
                ],
            }), ]
        )
    }
}