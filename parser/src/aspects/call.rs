use crate::aspects::group::GroupAspect;
use crate::aspects::literal::LiteralAspect;
use lexer::token::TokenType;

use crate::aspects::redirection::RedirectionAspect;
use crate::aspects::structure::StructureAspect;
use crate::ast::callable::Call;
use crate::ast::Expr;
use crate::moves::{eox, like, word_seps, MoveOperations};
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

        self.cursor.advance(word_seps()); //consume word separations
                                          // Continue reading arguments until we reach the end of the input or a closing punctuation
        while !self.cursor.is_at_end()
            && self
                .cursor
                .lookahead(word_seps().then(eox().or(like(TokenType::is_call_bound))))
                .is_none()
        {
            if self.is_at_redirection_sign() {
                return self.redirectable(Expr::Call(Call { arguments }));
            }
            arguments.push(self.call_argument()?);
            self.cursor.advance(word_seps()); //consume word separations
        }
        Ok(Expr::Call(Call { arguments }))
    }
}

impl<'a> Parser<'a> {
    /// special pivot method for argument methods
    fn call_argument(&mut self) -> ParseResult<Expr<'a>> {
        self.repos("Expected value")?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            TokenType::RoundedLeftBracket => Ok(Expr::Parenthesis(self.parenthesis()?)),
            TokenType::CurlyLeftBracket => Ok(Expr::Block(self.block()?)),
            TokenType::Identifier if self.is_at_constructor_start() => self.constructor(),
            _ => self.literal(),
        }
    }
}

#[cfg(test)]
mod tests {
    use context::source::Source;
    use pretty_assertions::assert_eq;

    use crate::ast::callable::Call;
    use crate::ast::value::Literal;
    use crate::ast::Expr;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::Parser;

    #[test]
    fn wrong_group_end() {
        let content = "ls )";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_next(),
            Err(ParseError {
                message: "expected end of expression or file".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unexpected,
            })
        );
    }

    #[test]
    fn not_in_call_is_literal() {
        let content = "echo how ! how are you !";
        let result = parse(Source::unknown(content)).expect("Failed to parse");
        assert_eq!(
            result,
            vec![Expr::Call(Call {
                arguments: vec![
                    Expr::Literal("echo".into()),
                    Expr::Literal("how".into()),
                    Expr::Literal("!".into()),
                    Expr::Literal("how".into()),
                    Expr::Literal("are".into()),
                    Expr::Literal("you".into()),
                    Expr::Literal("!".into())
                ]
            })]
        );
    }

    #[test]
    fn multiple_calls() {
        let source = Source::unknown("grep -E regex; echo test");
        let parsed = parse(source).expect("Failed to parse");
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
        let source = Source::unknown("g++ -std=c++20 \\\n-Wall \\\n-Wextra\\\n-Wpedantic");
        let parsed = parse(source).expect("Failed to parse");
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
        let source = Source::unknown("grep -E regex \\; echo test");
        let parsed = parse(source).expect("Failed to parse");
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
