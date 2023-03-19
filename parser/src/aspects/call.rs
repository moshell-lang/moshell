use crate::aspects::group::GroupAspect;
use crate::aspects::literal::{LiteralAspect, LiteralLeniency};
use lexer::token::{Token, TokenType};

use crate::aspects::r#type::TypeAspect;
use crate::aspects::redirection::RedirectionAspect;
use crate::err::ParseErrorKind;
use crate::moves::{eod, eox, like, lookahead, next, of_type, of_types, word_seps, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::call::{Call, ProgrammaticCall};
use ast::r#type::Type;
use ast::value::Literal;
use ast::Expr;

/// A parse aspect for command and function calls
pub trait CallAspect<'a> {
    /// Parses a raw call or a programmatic call.
    fn any_call(&mut self) -> ParseResult<Expr<'a>>;

    /// Attempts to parse the next raw call expression
    fn call(&mut self) -> ParseResult<Expr<'a>>;

    /// Parses a programmatic call.
    fn programmatic_call(&mut self) -> ParseResult<Expr<'a>>;

    /// Continues to parse a call expression from a known command name expression
    fn call_arguments(
        &mut self,
        command: Expr<'a>,
        tparams: Vec<Type<'a>>,
    ) -> ParseResult<Expr<'a>>;

    /// Checks if the cursor is at the start of a programmatic call.
    fn may_be_at_programmatic_call_start(&self) -> bool;
}

impl<'a> CallAspect<'a> for Parser<'a> {
    fn any_call(&mut self) -> ParseResult<Expr<'a>> {
        if !self.may_be_at_programmatic_call_start() {
            return self.call();
        }
        // We don't known if this is a programmatic call or a raw call yet.
        let identifier = self.cursor.peek();
        self.cursor.advance(next());
        let callee = Expr::Literal(Literal::from(identifier.value));
        let type_parameters = self.parse_type_parameter_list()?;
        if let Some(open_parenthesis) = self.cursor.advance(of_type(TokenType::RoundedLeftBracket))
        {
            self.delimiter_stack.push_back(open_parenthesis.clone());
            let arguments = self.parse_comma_separated_arguments(open_parenthesis)?;
            Ok(Expr::ProgrammaticCall(ProgrammaticCall {
                name: identifier.value,
                arguments,
                type_parameters,
            }))
        } else {
            self.call_arguments(callee, type_parameters)
        }
    }

    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let callee = self.next_value()?;
        let tparams = self.parse_type_parameter_list()?;
        self.call_arguments(callee, tparams)
    }

    fn programmatic_call(&mut self) -> ParseResult<Expr<'a>> {
        let name = self
            .cursor
            .force(of_type(TokenType::Identifier), "Expected function name.")?;
        let type_parameters = self.parse_type_parameter_list()?;
        let open_parenthesis = self.cursor.force(
            of_type(TokenType::RoundedLeftBracket),
            "Expected opening parenthesis.",
        )?;
        self.delimiter_stack.push_back(open_parenthesis.clone());
        let arguments = self.parse_comma_separated_arguments(open_parenthesis)?;
        Ok(Expr::ProgrammaticCall(ProgrammaticCall {
            name: name.value,
            arguments,
            type_parameters,
        }))
    }

    fn call_arguments(
        &mut self,
        callee: Expr<'a>,
        tparams: Vec<Type<'a>>,
    ) -> ParseResult<Expr<'a>> {
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
            return self.redirectable(Expr::Call(Call {
                arguments,
                type_parameters: tparams,
            }));
        }

        Ok(Expr::Call(Call {
            arguments,
            type_parameters: tparams,
        }))
    }

    fn may_be_at_programmatic_call_start(&self) -> bool {
        self.cursor
            .lookahead(of_type(TokenType::Identifier).and_then(of_types(&[
                TokenType::RoundedLeftBracket,
                TokenType::SquaredLeftBracket,
            ])))
            .is_some()
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
            TokenType::Identifier if self.may_be_at_programmatic_call_start() => {
                self.programmatic_call()
            }
            _ => self.literal(LiteralLeniency::Lenient),
        }
    }

    fn parse_comma_separated_arguments(
        &mut self,
        open_parenthesis: Token<'a>,
    ) -> ParseResult<Vec<Expr<'a>>> {
        // Read the args until a closing delimiter or a new non-escaped line is found.
        let mut args = Vec::new();
        loop {
            self.cursor.advance(word_seps());
            if self
                .cursor
                .advance(of_type(TokenType::RoundedRightBracket))
                .is_some()
            {
                self.delimiter_stack.pop_back();
                return Ok(args);
            }
            if self.cursor.lookahead(of_type(TokenType::Comma)).is_some() {
                self.expected("Expected argument.", ParseErrorKind::Unexpected)?;
            }
            args.push(self.value()?);
            self.cursor.advance(word_seps());

            // Check if the arg list is abnormally terminated.
            if self.cursor.lookahead(eox()).is_some() {
                self.expected(
                    "Expected closing parenthesis.",
                    ParseErrorKind::Unpaired(self.cursor.relative_pos(open_parenthesis.clone())),
                )?;
            }
            if self.cursor.lookahead(eod()).is_some() {
                self.expect_delimiter(TokenType::RoundedRightBracket)?;
                break;
            }
            self.cursor.force(
                word_seps().then(of_type(TokenType::Comma).or(lookahead(eod()))),
                "expected ','",
            )?;
        }

        Ok(args)
    }
}

#[cfg(test)]
mod tests {
    use context::source::Source;
    use pretty_assertions::assert_eq;

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use ast::call::{Call, ProgrammaticCall};
    use ast::r#type::Type;
    use ast::value::Literal;
    use ast::Expr;

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
        let content = "parse[int] x y";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_next(),
            Ok(Expr::Call(Call {
                arguments: vec![
                    Expr::Literal("parse".into()),
                    Expr::Literal("x".into()),
                    Expr::Literal("y".into())
                ],
                type_parameters: vec![Type {
                    name: "int",
                    params: Vec::new()
                }]
            }))
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
                type_parameters: vec![],
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
                    type_parameters: vec![]
                }),
                Expr::Call(Call {
                    arguments: vec![Expr::Literal("echo".into()), Expr::Literal("test".into())],
                    type_parameters: vec![],
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
                type_parameters: vec![],
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
                type_parameters: vec![]
            }),]
        )
    }

    #[test]
    fn empty_constructor() {
        let source = Source::unknown("Foo()");
        let source2 = Source::unknown("Foo( )");
        let expr = parse(source).expect("Failed to parse");
        let expr2 = parse(source2).expect("Failed to parse");
        let expected = vec![Expr::ProgrammaticCall(ProgrammaticCall {
            name: "Foo",
            arguments: vec![],
            type_parameters: vec![],
        })];
        assert_eq!(expr, expected);
        assert_eq!(expr2, expected);
    }

    #[test]
    fn parse_constructor() {
        let source = Source::unknown("Foo(a, 2, c)");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                name: "Foo",
                arguments: vec![
                    Expr::Literal("a".into()),
                    Expr::Literal(Literal {
                        lexeme: "2",
                        parsed: 2.into(),
                    }),
                    Expr::Literal("c".into()),
                ],
                type_parameters: vec![],
            })],
        );
    }

    #[test]
    fn constructor_with_newlines_and_space() {
        let source = Source::unknown("Foo( \\\nthis , \\\n  is,\\\nfine)");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                name: "Foo",
                arguments: vec![
                    Expr::Literal("this".into()),
                    Expr::Literal("is".into()),
                    Expr::Literal("fine".into()),
                ],
                type_parameters: vec![],
            })],
        );
    }

    #[test]
    fn constructor_accept_string_literals() {
        let source = Source::unknown("Foo('===\ntesting something\n===', c)");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                name: "Foo",
                arguments: vec![
                    Expr::Literal(Literal {
                        lexeme: "'===\ntesting something\n==='",
                        parsed: "===\ntesting something\n===".into(),
                    }),
                    Expr::Literal("c".into())
                ],
                type_parameters: vec![],
            }),]
        );
    }

    #[test]
    fn generic_constructor() {
        let source = Source::unknown("List[Str]('hi')");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                name: "List",
                arguments: vec![Expr::Literal(Literal {
                    lexeme: "'hi'",
                    parsed: "hi".into(),
                })],
                type_parameters: vec![Type {
                    name: "Str",
                    params: vec![],
                }],
            })],
        );
    }

    #[test]
    fn constructor_with_unpaired_parenthesis() {
        let content = "Foo(a, 2, c\n)";
        let source = Source::unknown(content);
        let expr: ParseResult<_> = parse(source).into();
        assert_eq!(
            expr,
            Err(ParseError {
                message: "Expected closing parenthesis.".into(),
                position: content.find('\n').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unpaired(content.find('(').map(|p| p..p + 1).unwrap())
            })
        )
    }

    #[test]
    fn constructor_exit_when_mismatched_bracket() {
        let content = "Foo(41 ]";
        let source = Source::unknown(content);
        let expr: ParseResult<_> = parse(source).into();
        assert_eq!(
            expr,
            Err(ParseError {
                message: "Mismatched closing delimiter.".into(),
                position: content.len() - 1..content.len(),
                kind: ParseErrorKind::Unpaired(content.find('(').map(|p| p..p + 1).unwrap())
            })
        )
    }
}
