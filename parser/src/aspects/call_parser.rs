use lexer::token::TokenType;

use crate::aspects::base_parser::BaseParser;
use crate::ast::callable::{Call, Redir, RedirFd, RedirOp};
use crate::ast::Expr;
use crate::parser::{ParseResult, Parser};

pub trait CallParser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>>;
    fn redirection(&mut self) -> ParseResult<Redir<'a>>;
}

impl<'a> CallParser<'a> for Parser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let mut arguments = vec![self.expression()?];
        let mut redirections = vec![];
        while !self.is_at_end() {
            match self.peek_token().token_type {
                TokenType::Ampersand | TokenType::Less | TokenType::Greater => {
                    redirections.push(self.redirection()?);
                }
                _ => arguments.push(self.expression()?),
            };
        }

        Ok(Expr::Call(Call {
            arguments,
            redirections,
        }))
    }

    fn redirection(&mut self) -> ParseResult<Redir<'a>> {
        let fd = match self.match_token(TokenType::Ampersand) {
            None => RedirFd::Default,
            Some(_) => RedirFd::Fd(
                self.expect_token(TokenType::IntLiteral, "Expected file descriptor.")?
                    .value
                    .parse()
                    .map_err(|_| self.mk_parse_error("Invalid file descriptor."))?,
            ),
        };
        let mut operator = match self.next_token_space_aware()?.token_type {
            TokenType::Less => RedirOp::Read,
            TokenType::Greater => match self.match_token_space_aware(TokenType::Greater) {
                None => RedirOp::Write,
                Some(_) => RedirOp::Append,
            },
            _ => Err(self.mk_parse_error("Expected redirection operator."))?,
        };
        if self.meet_token(TokenType::Ampersand) {
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
