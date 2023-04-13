use crate::aspects::group::GroupAspect;
use crate::aspects::literal::{LiteralAspect, LiteralLeniency};
use lexer::token::{Token, TokenType};

use crate::aspects::r#type::TypeAspect;
use crate::aspects::redirection::RedirectionAspect;
use crate::err::ParseErrorKind;
use crate::moves::{
    blanks, eod, eox, identifier_parenthesis, like, lookahead, next, of_type, of_types, spaces,
    MoveOperations,
};
use crate::parser::{ParseResult, Parser};
use ast::call::{Call, MethodCall, ProgrammaticCall};
use ast::lambda::LambdaDef;
use ast::r#type::Type;
use ast::value::Literal;
use ast::variable::TypedVariable;
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};

/// A parse aspect for command and function calls
pub trait CallAspect<'a> {
    /// Parses a raw call, a programmatic call or a lambda definition.
    fn any_call(&mut self) -> ParseResult<Expr<'a>>;

    /// Attempts to parse the next raw call expression
    fn call(&mut self) -> ParseResult<Expr<'a>>;

    /// Parses a programmatic call.
    fn programmatic_call(&mut self) -> ParseResult<Expr<'a>>;

    /// Parses a method call.
    fn method_call_on(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>>;

    /// Parse any function call or method call after an expression.
    fn expand_call_chain(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>>;

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
        // Equivalent to #may_be_at_programmatic_call_start, with an additional check for lambda definitions.
        if self
            .cursor
            .lookahead(
                of_type(TokenType::Identifier).and_then(
                    of_types(&[TokenType::RoundedLeftBracket, TokenType::SquaredLeftBracket])
                        .or(spaces().then(of_type(TokenType::FatArrow))),
                ),
            )
            .is_none()
        {
            return self.call();
        }
        // We don't known if this is a programmatic call, a raw call or a lambda definition yet.
        let identifier = self.cursor.peek();
        let value = identifier.value;
        self.cursor.advance(next());
        let callee = Expr::Literal(Literal {
            parsed: value.into(),
            segment: self.cursor.relative_pos(value),
        });
        let type_parameters = self.parse_type_parameter_list()?.0;
        if let Some(open_parenthesis) = self.cursor.advance(of_type(TokenType::RoundedLeftBracket))
        {
            self.delimiter_stack.push_back(open_parenthesis.clone());
            let (arguments, segment) = self.parse_comma_separated_arguments(open_parenthesis)?;
            Ok(Expr::ProgrammaticCall(ProgrammaticCall {
                name: value,
                arguments,
                type_parameters,
                segment: self.cursor.relative_pos(value).start..segment.end,
            }))
        } else if self
            .cursor
            .advance(spaces().then(of_type(TokenType::FatArrow)))
            .is_some()
        {
            let body = Box::new(self.value()?);
            let segment = self.cursor.relative_pos(identifier).start..body.segment().end;
            Ok(Expr::LambdaDef(LambdaDef {
                args: vec![TypedVariable {
                    name: value,
                    ty: None,
                    segment: self.cursor.relative_pos(value),
                }],
                body,
                segment,
            }))
        } else {
            self.call_arguments(callee, type_parameters)
        }
    }

    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let callee = self.next_value()?;
        let tparams = self.parse_type_parameter_list()?.0;
        self.call_arguments(callee, tparams)
    }

    fn programmatic_call(&mut self) -> ParseResult<Expr<'a>> {
        let name = self
            .cursor
            .force(of_type(TokenType::Identifier), "Expected function name.")?;
        let type_parameters = self.parse_type_parameter_list()?.0;
        let open_parenthesis = self.cursor.force(
            of_type(TokenType::RoundedLeftBracket),
            "Expected opening parenthesis.",
        )?;
        self.delimiter_stack.push_back(open_parenthesis.clone());
        let (arguments, segment) = self.parse_comma_separated_arguments(open_parenthesis)?;
        let segment = self.cursor.relative_pos(name.value).start..segment.end;
        Ok(Expr::ProgrammaticCall(ProgrammaticCall {
            name: name.value,
            arguments,
            type_parameters,
            segment,
        }))
    }

    fn method_call_on(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
        let dot = self.cursor.advance(of_type(TokenType::Dot));
        let name = if dot.is_some() {
            Some(
                self.cursor
                    .force(of_type(TokenType::Identifier), "Expected function name.")?,
            )
        } else {
            None
        };
        let type_parameters = self.parse_type_parameter_list()?.0;
        let open_parenthesis = self.cursor.force(
            of_type(TokenType::RoundedLeftBracket),
            "Expected opening parenthesis.",
        )?;
        self.delimiter_stack.push_back(open_parenthesis.clone());
        let (arguments, segment) = self.parse_comma_separated_arguments(open_parenthesis)?;
        let segment = dot
            .map(|d| self.cursor.relative_pos(d.value))
            .unwrap_or(expr.segment())
            .start..segment.end;
        Ok(Expr::MethodCall(MethodCall {
            source: Box::new(expr),
            name: name.map(|n| n.value),
            arguments,
            type_parameters,
            segment,
        }))
    }

    fn expand_call_chain(&mut self, mut expr: Expr<'a>) -> ParseResult<Expr<'a>> {
        while self
            .cursor
            .lookahead(
                of_type(TokenType::RoundedLeftBracket)
                    .or(blanks().then(of_type(TokenType::Dot).and_then(identifier_parenthesis()))),
            )
            .is_some()
        {
            self.cursor.advance(blanks());
            expr = self.method_call_on(expr)?;
        }
        Ok(expr)
    }

    fn call_arguments(
        &mut self,
        callee: Expr<'a>,
        tparams: Vec<Type<'a>>,
    ) -> ParseResult<Expr<'a>> {
        let mut arguments = vec![callee];

        self.cursor.advance(spaces()); //consume word separations
                                       // Continue reading arguments until we reach the end of the input or a closing punctuation
        while !self.cursor.is_at_end()
            && self
                .cursor
                .lookahead(spaces().then(eox().or(like(TokenType::is_call_bound))))
                .is_none()
        {
            arguments.push(self.call_argument()?);
            self.cursor.advance(spaces()); //consume word separations
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
        self.cursor.lookahead(identifier_parenthesis()).is_some()
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
    ) -> ParseResult<(Vec<Expr<'a>>, SourceSegment)> {
        // Read the args until a closing delimiter or a new non-escaped line is found.
        let mut args = Vec::new();
        let mut segment = self.cursor.relative_pos(open_parenthesis.clone());
        loop {
            self.cursor.advance(spaces());
            if let Some(closing_parenthesis) =
                self.cursor.advance(of_type(TokenType::RoundedRightBracket))
            {
                self.delimiter_stack.pop_back();
                segment.end = self.cursor.relative_pos(closing_parenthesis).end;
                return Ok((args, segment));
            }
            if self.cursor.lookahead(of_type(TokenType::Comma)).is_some() {
                self.expected("Expected argument.", ParseErrorKind::Unexpected)?;
            }
            args.push(self.value()?);
            self.cursor.advance(spaces());

            // Check if the arg list is abnormally terminated.
            if self.cursor.lookahead(eox()).is_some() {
                self.expected(
                    "Expected closing parenthesis.",
                    ParseErrorKind::Unpaired(self.cursor.relative_pos(open_parenthesis.clone())),
                )?;
            }
            if self.cursor.lookahead(eod()).is_some() {
                let closing_parenthesis = self.expect_delimiter(TokenType::RoundedRightBracket)?;
                segment.end = self.cursor.relative_pos_ctx(closing_parenthesis).end;
                break;
            }
            self.cursor.force(
                spaces().then(of_type(TokenType::Comma).or(lookahead(eod()))),
                "expected ','",
            )?;
        }

        Ok((args, segment))
    }
}

#[cfg(test)]
mod tests {
    use context::source::{Source, SourceSegmentHolder};
    use pretty_assertions::assert_eq;

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use crate::source::{literal, literal_nth};
    use ast::call::{Call, ProgrammaticCall};
    use ast::r#type::{SimpleType, Type};
    use ast::value::Literal;
    use ast::Expr;
    use context::str_find::{find_between, find_in};

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
        let source = Source::unknown("parse[int] x y");
        assert_eq!(
            Parser::new(source.clone()).parse_next(),
            Ok(Expr::Call(Call {
                arguments: vec![
                    literal(source.source, "parse"),
                    literal(source.source, "x"),
                    literal(source.source, "y"),
                ],
                type_parameters: vec![Type::Simple(SimpleType {
                    name: "int",
                    params: Vec::new(),
                    segment: find_in(source.source, "int")
                })]
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
                    literal(content, "echo"),
                    literal(content, "how"),
                    literal(content, "!"),
                    literal_nth(content, "how", 1),
                    literal(content, "are"),
                    literal(content, "you"),
                    literal_nth(content, "!", 1),
                ],
                type_parameters: vec![],
            })]
        );
    }

    #[test]
    fn multiple_calls() {
        let source = Source::unknown("grep -E regex; echo test");
        let parsed = parse(source.clone()).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![
                Expr::Call(Call {
                    arguments: vec![
                        literal(source.source, "grep"),
                        literal(source.source, "-E"),
                        literal(source.source, "regex")
                    ],
                    type_parameters: vec![]
                }),
                Expr::Call(Call {
                    arguments: vec![
                        literal(source.source, "echo"),
                        literal(source.source, "test")
                    ],
                    type_parameters: vec![],
                }),
            ]
        )
    }

    #[test]
    fn multiline_call() {
        let source = Source::unknown("g++ -std=c++20 \\\n-Wall \\\n-Wextra\\\n-Wpedantic");
        let parsed = parse(source.clone()).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![Expr::Call(Call {
                arguments: vec![
                    literal(source.source, "g++"),
                    literal(source.source, "-std=c++20"),
                    literal(source.source, "-Wall"),
                    literal(source.source, "-Wextra"),
                    literal(source.source, "-Wpedantic"),
                ],
                type_parameters: vec![],
            }),]
        )
    }

    #[test]
    fn escaped_call() {
        let source = Source::unknown("grep -E regex \\; echo test");
        let parsed = parse(source.clone()).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![Expr::Call(Call {
                arguments: vec![
                    literal(source.source, "grep"),
                    literal(source.source, "-E"),
                    literal(source.source, "regex"),
                    literal(source.source, ";"),
                    literal(source.source, "echo"),
                    literal(source.source, "test"),
                ],
                type_parameters: vec![]
            }),]
        )
    }

    #[test]
    fn empty_constructor() {
        let source = Source::unknown("Foo()");
        let source2 = Source::unknown("Foo( )");
        let expr = parse(source.clone()).expect("Failed to parse");
        let expr2 = parse(source2.clone()).expect("Failed to parse");
        let mut expected = ProgrammaticCall {
            name: "Foo",
            arguments: vec![],
            type_parameters: vec![],
            segment: source.segment(),
        };
        assert_eq!(expr, vec![Expr::ProgrammaticCall(expected.clone())]);
        expected.segment = source2.segment();
        assert_eq!(expr2, vec![Expr::ProgrammaticCall(expected)]);
    }

    #[test]
    fn parse_constructor() {
        let source = Source::unknown("Foo(a, 2, c)");
        let expr = parse(source.clone()).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                name: "Foo",
                arguments: vec![
                    literal(source.source, "a"),
                    Expr::Literal(Literal {
                        parsed: 2.into(),
                        segment: find_in(source.source, "2")
                    }),
                    literal(source.source, "c"),
                ],
                type_parameters: vec![],
                segment: source.segment(),
            })],
        );
    }

    #[test]
    fn constructor_with_newlines_and_space() {
        let source = Source::unknown("Foo( \\\nthis , \\\n  is,\\\nfine)");
        let expr = parse(source.clone()).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                name: "Foo",
                arguments: vec![
                    literal(source.source, "this"),
                    literal_nth(source.source, "is", 1),
                    literal(source.source, "fine"),
                ],
                type_parameters: vec![],
                segment: source.segment(),
            })],
        );
    }

    #[test]
    fn constructor_accept_string_literals() {
        let source = Source::unknown("Foo('===\ntesting something\n===', c)");
        let expr = parse(source.clone()).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                name: "Foo",
                arguments: vec![
                    Expr::Literal(Literal {
                        parsed: "===\ntesting something\n===".into(),
                        segment: find_between(source.source, "'", "'")
                    }),
                    literal(source.source, "c"),
                ],
                type_parameters: vec![],
                segment: source.segment()
            }),]
        );
    }

    #[test]
    fn generic_constructor() {
        let source = Source::unknown("List[Str]('hi')");
        let expr = parse(source.clone()).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                name: "List",
                arguments: vec![Expr::Literal(Literal {
                    parsed: "hi".into(),
                    segment: find_in(source.source, "'hi'")
                })],
                type_parameters: vec![Type::Simple(SimpleType {
                    name: "Str",
                    params: vec![],
                    segment: find_in(source.source, "Str")
                })],
                segment: source.segment(),
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
