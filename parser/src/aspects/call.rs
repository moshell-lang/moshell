use crate::aspects::group::GroupAspect;
use crate::aspects::literal::LiteralAspect;
use lexer::token::TokenType;
use lexer::token::TokenType::{Comma, SquaredLeftBracket, SquaredRightBracket};

use crate::aspects::redirection::RedirectionAspect;
use crate::aspects::structure::StructureAspect;
use ast::callable::Call;
use ast::Expr;
use crate::err::ParseErrorKind;
use crate::moves::{eox, like, of_type, spaces, word_seps, MoveOperations};
use crate::parser::{ParseResult, Parser};

/// A parse aspect for command and function calls
pub trait CallAspect<'a> {
    /// Attempts to parse the next call expression
    fn call(&mut self) -> ParseResult<Expr<'a>>;

    /// Continues to parse a call expression from a known command name expression
    fn call_arguments(&mut self, command: Expr<'a>, tparams: Vec<&'a str>)
        -> ParseResult<Expr<'a>>;
}

impl<'a> CallAspect<'a> for Parser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let callee = self.next_value()?;
        let tparams = self.call_type_parameters()?;
        self.call_arguments(callee, tparams)
    }

    fn call_arguments(&mut self, callee: Expr<'a>, tparams: Vec<&'a str>) -> ParseResult<Expr<'a>> {
        let mut arguments = vec![callee];

        self.cursor.advance(word_seps()); //consume word separations
                                          // Continue reading arguments until we reach the end of the input or a closing punctuation
        while !self.cursor.is_at_end()
            && self
                .cursor
                .lookahead(word_seps().then(eox().or(like(TokenType::is_call_bound))))
                .is_none()
        {
            arguments.push(self.call_argument()?);
            self.cursor.advance(word_seps()); //consume word separations
        }

        if self.is_at_redirection_sign() {
            return self.redirectable(Expr::Call(Call { arguments, tparams }));
        }

        Ok(Expr::Call(Call { arguments, tparams }))
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

    fn call_type_parameters(&mut self) -> ParseResult<Vec<&'a str>> {
        let start = match self.cursor.advance(of_type(SquaredLeftBracket)) {
            Some(start) => start,
            None => return Ok(Vec::new()),
        };

        if self
            .cursor
            .advance(word_seps().then(of_type(SquaredRightBracket)))
            .is_some()
        {
            return self.expected_with(
                "unexpected empty type parameter list",
                start..self.cursor.peek(),
                ParseErrorKind::Unexpected,
            );
        }
        let mut tparams = vec![self.call_type_parameter()?];

        while self
            .cursor
            .advance(word_seps().then(of_type(SquaredRightBracket)))
            .is_none()
        {
            self.cursor.force(
                word_seps().then(of_type(Comma)),
                "A comma or a closing bracket was expected here",
            )?;
            tparams.push(self.call_type_parameter()?);
        }

        Ok(tparams)
    }

    fn call_type_parameter(&mut self) -> ParseResult<&'a str> {
        self.cursor.advance(spaces()); //consume spaces
        let first = self.cursor.next()?;

        if first.token_type != TokenType::Identifier {
            return self.expected_with(
                &format!("'{}' is not a valid type identifier.", first.value),
                first,
                ParseErrorKind::Unexpected,
            );
        }
        Ok(first.value)
    }
}

#[cfg(test)]
mod tests {
    use context::source::Source;
    use pretty_assertions::assert_eq;

    use ast::callable::Call;
    use ast::value::Literal;
    use ast::Expr;
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
    fn call_with_type_parameter() {
        let content = "parse[Int] x y";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_next(),
            Ok(Expr::Call(Call {
                arguments: vec![
                    Expr::Literal("parse".into()),
                    Expr::Literal("x".into()),
                    Expr::Literal("y".into())
                ],
                tparams: vec!["Int"]
            }))
        );
    }

    #[test]
    fn call_with_type_parameters() {
        let content = "complex[  int , float  ,  Structure  ] x y";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_next(),
            Ok(Expr::Call(Call {
                arguments: vec![
                    Expr::Literal("complex".into()),
                    Expr::Literal("x".into()),
                    Expr::Literal("y".into())
                ],
                tparams: vec!["int", "float", "Structure"]
            }))
        );
    }

    #[test]
    fn call_with_empty_type_parameters() {
        let content = "complex[    ] x y";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_next(),
            Err(ParseError {
                message: "unexpected empty type parameter list".to_string(),
                kind: ParseErrorKind::Unexpected,
                position: content
                    .find("[    ]")
                    .map(|i| i..i + "[    ]".len() + 1)
                    .unwrap()
            })
        );
    }

    #[test]
    fn call_with_not_identifier_parameter() {
        let content = "complex[  @  ] x y";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_next(),
            Err(ParseError {
                message: "'@' is not a valid type identifier.".to_string(),
                kind: ParseErrorKind::Unexpected,
                position: content.find('@').map(|i| i..i + 1).unwrap()
            })
        );
    }

    #[test]
    fn call_with_bad_commas() {
        let content = "complex[x y] x y";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_next(),
            Err(ParseError {
                message: "A comma or a closing bracket was expected here".to_string(),
                kind: ParseErrorKind::Unexpected,
                position: content.find('y').map(|i| i..i + 1).unwrap()
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
                    Expr::Literal("!".into()),
                ],
                tparams: vec![],
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
                    tparams: vec![]
                }),
                Expr::Call(Call {
                    arguments: vec![Expr::Literal("echo".into()), Expr::Literal("test".into())],
                    tparams: vec![],
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
                tparams: vec![],
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
                tparams: vec![]
            }),]
        )
    }
}
