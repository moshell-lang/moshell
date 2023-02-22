use lexer::token::TokenType;

use crate::ast::callable::{Call, Pipeline, Redir, RedirFd, RedirOp};
use crate::ast::Expr;
use crate::moves::{next, of_type, of_types, space, MoveOperations};
use crate::parser::{ParseResult, Parser};

pub trait CallParser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>>;
    fn pipeline(&mut self, first_call: Call<'a>) -> ParseResult<Expr<'a>>;
    fn redirection(&mut self) -> ParseResult<Redir<'a>>;
}

impl<'a> CallParser<'a> for Parser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let mut arguments = vec![self.expression()?];
        let mut redirections = vec![];
        while !self.cursor.is_at_end() {
            self.cursor.advance(space());
            match self.cursor.peek().token_type {
                TokenType::Ampersand | TokenType::Less | TokenType::Greater => {
                    redirections.push(self.redirection()?);
                }
                TokenType::Pipe => {
                    return self.pipeline(Call {
                        arguments,
                        redirections,
                    });
                }
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

        Ok(Expr::Call(Call {
            arguments,
            redirections,
        }))
    }

    fn pipeline(&mut self, first_call: Call<'a>) -> ParseResult<Expr<'a>> {
        let mut commands = vec![first_call];
        while self
            .cursor
            .advance(space().then(of_type(TokenType::Pipe)))
            .is_some()
        {
            match self.call()? {
                Expr::Call(call) => commands.push(call),
                Expr::Pipeline(pipeline) => commands.extend(pipeline.commands),
                _ => Err(self.mk_parse_error("Expected a command."))?,
            }
        }
        Ok(Expr::Pipeline(Pipeline {
            commands,
            negation: false,
        }))
    }

    fn redirection(&mut self) -> ParseResult<Redir<'a>> {
        self.cursor.advance(space());
        let mut token = self.cursor.next()?;
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
    use crate::ast::callable::{Call, Redir, RedirFd, RedirOp};
    use crate::ast::literal::Literal;
    use crate::ast::Expr;
    use crate::parser::Parser;
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
            Expr::Call(Call {
                arguments: vec![Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "ls"),
                    parsed: "ls".into(),
                })],
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
            Expr::Call(Call {
                arguments: vec![Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "ls"),
                    parsed: "ls".into(),
                })],
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
}
