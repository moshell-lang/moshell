use ast::call::{Pipeline, Redir, RedirFd, RedirOp, Redirected};
use ast::substitution::{Substitution, SubstitutionKind};
use ast::{substitution, Expr};
use context::source::SourceSegmentHolder;
use lexer::token::{Token, TokenType};

use crate::err::ParseErrorKind;
use crate::moves::{eox, next, of_type, of_types, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    pub(crate) fn pipeline(&mut self, first_call: Expr) -> ParseResult<Expr> {
        let mut commands = vec![first_call];
        // Continue as long as we have a pipe
        while self
            .cursor
            .advance(spaces().then(of_type(TokenType::Bar)))
            .is_some()
        {
            match self.statement()? {
                Expr::Pipeline(pipeline) => commands.extend(pipeline.commands),
                call => commands.push(call),
            }
        }
        Ok(Expr::Pipeline(Pipeline { commands }))
    }

    fn redirection(&mut self) -> ParseResult<Redir> {
        self.cursor.advance(spaces());
        let backtick = self.cursor.advance(of_type(TokenType::Backtick));
        let mut token = self.cursor.next()?;
        let start = backtick.as_ref().unwrap_or(&token).clone();
        // Parse if present the redirected file descriptor
        let fd = match token.token_type {
            TokenType::Ampersand => {
                token = self.cursor.next()?;
                RedirFd::Wildcard
            }
            TokenType::IntLiteral => {
                let redir = RedirFd::Fd(token.text(self.source).parse().map_err(|_| {
                    self.mk_parse_error(
                        "Invalid file descriptor.",
                        token.span,
                        ParseErrorKind::InvalidFormat,
                    )
                })?);
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
            _ => self.expected_with(
                "Expected redirection operator.",
                token.span,
                ParseErrorKind::Expected("< >".to_string()),
            )?,
        };

        // Parse file descriptor duplication and update the operator
        if self.cursor.advance(of_type(TokenType::Ampersand)).is_some() {
            operator = match operator {
                RedirOp::Read => RedirOp::FdIn,
                RedirOp::Write => RedirOp::FdOut,
                _ => self.expected(
                    "Invalid redirection operator.",
                    ParseErrorKind::Expected("< or >".to_string()),
                )?,
            };
        }
        if backtick.is_some() {
            self.cursor.advance(of_type(TokenType::Backtick));
        }

        let operand = self.call_argument()?;
        let segment = start.span.start..operand.segment().end;
        Ok(Redir {
            fd,
            operator,
            operand,
            segment,
        })
    }

    pub(crate) fn redirectable(&mut self, expr: Expr) -> ParseResult<Expr> {
        let mut redirections = vec![];
        self.cursor.advance(spaces());

        while self.cursor.lookahead(eox()).is_none() {
            match self.cursor.peek().token_type {
                TokenType::Less | TokenType::Greater | TokenType::Backtick => {
                    redirections.push(self.redirection()?);
                }
                // Detect redirections with a specific file descriptor, or with a wildcard file descriptor
                // To be a redirection, it must immediately be followed by a '<' or '>'
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

        Ok(if redirections.is_empty() {
            expr
        } else {
            Expr::Redirected(Redirected {
                expr: Box::new(expr),
                redirections,
            })
        })
    }

    pub(crate) fn is_at_redirection_sign(&self) -> bool {
        let pivot = self.cursor.peek();
        match pivot.token_type {
            TokenType::Ampersand | TokenType::Less | TokenType::Greater => true,
            //search for '>' or '<' in case of std-determined redirection sign (ex: 2>>)
            _ => self
                .cursor
                .lookahead(next().then(of_types(&[TokenType::Less, TokenType::Greater])))
                .is_some(),
        }
    }

    pub(crate) fn process_substitution(&mut self, token: Token) -> ParseResult<Expr> {
        let mut underlying = self.subshell()?;
        underlying.segment = token.span.start..underlying.segment.end;
        Ok(Expr::Substitution(Substitution {
            underlying,
            kind: SubstitutionKind::Process {
                direction: if token.token_type == TokenType::Less {
                    substitution::Direction::Input
                } else {
                    substitution::Direction::Output
                },
            },
        }))
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use ast::call::{Call, Redir, RedirFd, RedirOp, Redirected};
    use ast::control_flow::Loop;
    use ast::group::{Block, Subshell};
    use ast::substitution::{Substitution, SubstitutionKind};
    use ast::value::Literal;
    use ast::Expr;
    use context::str_find::find_in;

    use crate::parse;
    use crate::parser::Parser;
    use crate::source::literal;

    #[test]
    fn expr_redirection() {
        let source = "{ls; cd;} `>` /tmp/out";
        let parsed = parse(source).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![Expr::Redirected(Redirected {
                expr: Box::new(Expr::Block(Block {
                    expressions: vec![
                        Expr::Call(Call {
                            arguments: vec![literal(source, "ls")],
                        }),
                        Expr::Call(Call {
                            arguments: vec![literal(source, "cd")],
                        }),
                    ],
                    segment: find_in(source, "{ls; cd;}")
                })),
                redirections: vec![Redir {
                    operator: RedirOp::Write,
                    fd: RedirFd::Default,
                    operand: literal(source, "/tmp/out"),
                    segment: find_in(source, "`>` /tmp/out"),
                }],
            })]
        );
    }

    #[test]
    fn call_redirection() {
        let source = "ls> /tmp/out";
        let parsed = Parser::new(source).call().expect("Failed to parse");
        assert_eq!(
            parsed,
            Expr::Redirected(Redirected {
                expr: Box::new(Expr::Call(Call {
                    arguments: vec![literal(source, "ls")],
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::Write,
                    operand: literal(source, "/tmp/out"),
                    segment: find_in(source, "> /tmp/out"),
                }],
            })
        );
    }

    #[test]
    fn dupe_fd() {
        let source = "ls>&2";
        let parsed = Parser::new(source).call().expect("Failed to parse");
        assert_eq!(
            parsed,
            Expr::Redirected(Redirected {
                expr: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal(Literal {
                        parsed: "ls".into(),
                        segment: find_in(source, "ls")
                    })],
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::FdOut,
                    operand: Expr::Literal(Literal {
                        parsed: 2.into(),
                        segment: find_in(source, "2")
                    }),
                    segment: find_in(source, ">&2"),
                }],
            })
        );
    }

    #[test]
    fn process_substitution() {
        let source = "diff <(echo before) - < <(/bin/echo after)";
        let parsed = parse(source).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![Expr::Redirected(Redirected {
                expr: Box::new(Expr::Call(Call {
                    arguments: vec![
                        literal(source, "diff"),
                        Expr::Substitution(Substitution {
                            underlying: Subshell {
                                expressions: vec![Expr::Call(Call {
                                    arguments: vec![
                                        literal(source, "echo"),
                                        literal(source, "before")
                                    ],
                                })],
                                segment: find_in(source, "<(echo before)"),
                            },
                            kind: SubstitutionKind::Process {
                                direction: ast::substitution::Direction::Input
                            }
                        }),
                        literal(source, "-"),
                    ],
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::Read,
                    operand: Expr::Substitution(Substitution {
                        underlying: Subshell {
                            expressions: vec![Expr::Call(Call {
                                arguments: vec![
                                    literal(source, "/bin/echo"),
                                    literal(source, "after")
                                ],
                            })],
                            segment: find_in(source, "<(/bin/echo after)"),
                        },
                        kind: SubstitutionKind::Process {
                            direction: ast::substitution::Direction::Input
                        }
                    }),
                    segment: find_in(source, "< <(/bin/echo after)"),
                }],
            })],
        );
    }

    #[test]
    fn loop_process_substitution() {
        // Maybe lint if written as `loop {} `<` <(ls)` ?
        let source = "{ loop {} } `<` <(ls)";
        let parsed = parse(source).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![Expr::Redirected(Redirected {
                expr: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Loop(Loop {
                        body: Box::new(Expr::Block(Block {
                            expressions: vec![],
                            segment: find_in(source, "{}"),
                        })),
                        segment: find_in(source, "loop {}"),
                    })],
                    segment: find_in(source, "{ loop {} }"),
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::Read,
                    operand: Expr::Substitution(Substitution {
                        underlying: Subshell {
                            expressions: vec![Expr::Call(Call {
                                arguments: vec![literal(source, "ls")],
                            })],
                            segment: find_in(source, "<(ls)"),
                        },
                        kind: SubstitutionKind::Process {
                            direction: ast::substitution::Direction::Input
                        }
                    }),
                    segment: find_in(source, "`<` <(ls)"),
                }],
            })]
        );
    }
}
