use ast::call::{Pipeline, Redir, RedirFd, RedirOp, Redirected};
use ast::Expr;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType;

use crate::err::ParseErrorKind;
use crate::moves::{eox, next, of_type, of_types, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};

pub(crate) trait RedirectionAspect<'a> {
    /// Attempts to parse the next pipeline expression
    /// inputs an "end of call" statements to determine where the call can stop.
    fn pipeline(&mut self, first_call: Expr<'a>) -> ParseResult<Expr<'a>>;
    /// Attempts to parse the next redirection
    fn redirection(&mut self) -> ParseResult<Redir<'a>>;
    /// Associates any potential redirections to a redirectable expression
    fn redirectable(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>>;

    /// Tests if the current and subsequent tokens can be part of a redirection expression.
    ///
    /// Note that this method will also treat detached expressions as redirections.
    fn is_at_redirection_sign(&self) -> bool;
}

impl<'a> RedirectionAspect<'a> for Parser<'a> {
    fn pipeline(&mut self, first_call: Expr<'a>) -> ParseResult<Expr<'a>> {
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

    fn redirection(&mut self) -> ParseResult<Redir<'a>> {
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
                let redir = RedirFd::Fd(token.value.parse().map_err(|_| {
                    self.mk_parse_error(
                        "Invalid file descriptor.",
                        token,
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
                token,
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
        let segment = self.cursor.relative_pos_ctx(start).start..operand.segment().end;
        Ok(Redir {
            fd,
            operator,
            operand,
            segment,
        })
    }

    fn redirectable(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
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

    fn is_at_redirection_sign(&self) -> bool {
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
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use ast::call::{Call, Redir, RedirFd, RedirOp, Redirected};
    use ast::group::Block;
    use ast::value::Literal;
    use ast::Expr;
    use context::source::Source;
    use context::str_find::find_in;

    use crate::aspects::call::CallAspect;
    use crate::parse;
    use crate::parser::Parser;
    use crate::source::literal;

    #[test]
    fn expr_redirection() {
        let content = "{ls; cd;} `>` /tmp/out";
        let source = Source::unknown(content);
        let parsed = parse(source).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![Expr::Redirected(Redirected {
                expr: Box::new(Expr::Block(Block {
                    expressions: vec![
                        Expr::Call(Call {
                            arguments: vec![literal(source.source, "ls")],
                        }),
                        Expr::Call(Call {
                            arguments: vec![literal(source.source, "cd")],
                        }),
                    ],
                    segment: find_in(content, "{ls; cd;}")
                })),
                redirections: vec![Redir {
                    operator: RedirOp::Write,
                    fd: RedirFd::Default,
                    operand: literal(source.source, "/tmp/out"),
                    segment: find_in(content, "`>` /tmp/out"),
                }],
            }),]
        );
    }

    #[test]
    fn call_redirection() {
        let content = "ls> /tmp/out";
        let source = Source::unknown(content);
        let parsed = Parser::new(source).call().expect("Failed to parse");
        assert_eq!(
            parsed,
            Expr::Redirected(Redirected {
                expr: Box::new(Expr::Call(Call {
                    arguments: vec![literal(source.source, "ls")],
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::Write,
                    operand: literal(source.source, "/tmp/out"),
                    segment: find_in(content, "> /tmp/out"),
                }],
            })
        );
    }

    #[test]
    fn dupe_fd() {
        let content = "ls>&2";
        let source = Source::unknown(content);
        let parsed = Parser::new(source).call().expect("Failed to parse");
        assert_eq!(
            parsed,
            Expr::Redirected(Redirected {
                expr: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal(Literal {
                        parsed: "ls".into(),
                        segment: find_in(content, "ls")
                    })],
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::FdOut,
                    operand: Expr::Literal(Literal {
                        parsed: 2.into(),
                        segment: find_in(content, "2")
                    }),
                    segment: find_in(content, ">&2"),
                }],
            })
        );
    }
}
