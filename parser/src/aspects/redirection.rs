use crate::aspects::call::CallAspect;
use crate::err::ParseErrorKind;
use crate::moves::{eox, like, next, of_type, of_types, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::call::{Pipeline, Redir, RedirFd, RedirOp, Redirected};
use ast::Expr;
use lexer::token::TokenType;
use lexer::token::TokenType::{BackSlash, DoubleQuote, Quote};

pub(crate) trait RedirectionAspect<'a> {
    /// Attempts to parse the next pipeline expression
    /// inputs an "end of call" statements to determine where the call can stop.
    fn pipeline(&mut self, first_call: Expr<'a>) -> ParseResult<Expr<'a>>;
    /// Attempts to parse the next redirection
    fn redirection(&mut self) -> ParseResult<Redir<'a>>;
    /// Associates any potential redirections to a redirectable expression
    fn redirectable(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>>;

    /// Tests if the current and subsequent tokens can be part of a redirection expression
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
            match self.call()? {
                Expr::Pipeline(pipeline) => commands.extend(pipeline.commands),
                call => commands.push(call),
            }
        }
        Ok(Expr::Pipeline(Pipeline { commands }))
    }

    fn redirection(&mut self) -> ParseResult<Redir<'a>> {
        self.cursor.advance(spaces());
        let mut token = self.cursor.next()?;
        println!("token: {:?}", token);
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
            _ => self.expected(
                "Expected redirection operator.",
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

        let operand = self.next_value()?;
        println!("peek: {:?}", self.cursor.peek());
        Ok(Redir {
            fd,
            operator,
            operand,
            segment: self.cursor.relative_pos_ctx(token..self.cursor.peek()),
        })
    }

    fn redirectable(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
        let mut redirections = vec![];
        self.cursor.advance(spaces());

        while self.cursor.lookahead(eox()).is_none() {
            match self.cursor.peek().token_type {
                //add a guard to ensure that the ampersand is not followed by space which should later lead to a detached expression
                TokenType::Ampersand
                    if self
                        .cursor
                        .lookahead(next().then(spaces().or(like(TokenType::is_call_bound))))
                        .is_none() =>
                {
                    redirections.push(self.redirection()?);
                }

                TokenType::Less | TokenType::Greater => {
                    redirections.push(self.redirection()?);
                }

                TokenType::Bar => {
                    return self.pipeline(expr);
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
        if self
            .cursor
            .lookahead(of_types(&[BackSlash, Quote, DoubleQuote]))
            .is_some()
        {
            return false;
        }

        let pivot = self.cursor.peek(); //repeat() always succeeds
        match pivot.token_type {
            TokenType::Ampersand | TokenType::Less | TokenType::Greater | TokenType::Bar => true,
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
    use context::source::Source;
    use pretty_assertions::assert_eq;

    use crate::aspects::call::CallAspect;
    use crate::err::find_in;
    use crate::parse;
    use crate::parser::Parser;
    use ast::call::{Call, Redir, RedirFd, RedirOp, Redirected};
    use ast::group::Block;
    use ast::value::Literal;
    use ast::Expr;

    #[test]
    fn expr_redirection() {
        let content = "{ls; cd;} > /tmp/out";
        let source = Source::unknown(content);
        let parsed = parse(source).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![Expr::Redirected(Redirected {
                expr: Box::new(Expr::Block(Block {
                    expressions: vec![
                        Expr::Call(Call {
                            arguments: vec![Expr::Literal("ls".into())],
                            type_parameters: vec![],
                        }),
                        Expr::Call(Call {
                            arguments: vec![Expr::Literal("cd".into())],
                            type_parameters: vec![],
                        }),
                    ]
                })),
                redirections: vec![Redir {
                    operator: RedirOp::Write,
                    fd: RedirFd::Default,
                    operand: Expr::Literal("/tmp/out".into()),
                    segment: find_in(content, "> /tmp/out"),
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
                    arguments: vec![Expr::Literal("ls".into())],
                    type_parameters: vec![],
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::Write,
                    operand: Expr::Literal("/tmp/out".into()),
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
                    arguments: vec![Expr::Literal("ls".into())],
                    type_parameters: vec![],
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::FdOut,
                    operand: Expr::Literal(Literal {
                        lexeme: "2",
                        parsed: 2.into(),
                    }),
                    segment: find_in(content, ">&2"),
                }],
            })
        );
    }
}
