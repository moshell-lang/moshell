use ast::call::{Call, MethodCall, ProgrammaticCall};
use ast::lambda::LambdaDef;
use ast::r#type::Type;
use ast::r#use::InclusionPathItem;
use ast::value::Literal;
use ast::variable::TypedVariable;
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};
use lexer::token::{Token, TokenType};

use crate::aspects::group::GroupAspect;
use crate::aspects::literal::{LiteralAspect, LiteralLeniency};
use crate::aspects::modules::ModulesAspect;
use crate::aspects::r#type::TypeAspect;
use crate::aspects::redirection::RedirectionAspect;
use crate::err::ParseErrorKind;
use crate::moves::{
    any, blanks, eog, identifier_parenthesis, like, line_end, lookahead, of_type, of_types, spaces,
    MoveOperations,
};
use crate::parser::{ensure_empty, ParseResult, Parser};

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
    fn call_arguments(&mut self, command: Expr<'a>) -> ParseResult<Expr<'a>>;

    /// Checks if the cursor is at the start of a programmatic call.
    fn may_be_at_programmatic_call_start(&self) -> bool;
}

impl<'a> CallAspect<'a> for Parser<'a> {
    fn any_call(&mut self) -> ParseResult<Expr<'a>> {
        let mut path = self.parse_inclusion_path()?;

        // Equivalent to #may_be_at_programmatic_call_start, with an additional check for lambda definitions.
        if self
            .cursor
            .lookahead(
                any().and_then(
                    of_types(&[
                        TokenType::RoundedLeftBracket,
                        TokenType::SquaredLeftBracket,
                        TokenType::ColonColon,
                    ])
                    .or(spaces().then(of_type(TokenType::FatArrow))),
                ),
            )
            .is_none()
        {
            ensure_empty(
                "Command calls cannot have inclusion path",
                path,
                InclusionPathItem::segment,
            )?;
            return self.call();
        }
        // We don't known if this is a programmatic call, a raw call or a lambda definition yet.

        let identifier = self.cursor.next()?;
        let name = identifier.value;
        let name_segment = self.cursor.relative_pos(name);
        let callee = Expr::Literal(Literal {
            parsed: name.into(),
            segment: name_segment.clone(),
        });
        let type_parameters = self.parse_type_parameter_list()?.0;
        if let Some(open_parenthesis) = self.cursor.advance(of_type(TokenType::RoundedLeftBracket))
        {
            if identifier.token_type != TokenType::Identifier {
                return self.expected_with(
                    "Expected function name.",
                    identifier,
                    ParseErrorKind::Expected("valid function identifier".to_string()),
                );
            }
            self.delimiter_stack.push_back(open_parenthesis.clone());
            let (arguments, args_segment) =
                self.parse_comma_separated_arguments(open_parenthesis)?;

            let start = path
                .first()
                .map(InclusionPathItem::segment)
                .unwrap_or_else(|| name_segment.clone());

            path.push(InclusionPathItem::Symbol(name, name_segment));
            Ok(Expr::ProgrammaticCall(ProgrammaticCall {
                path,
                arguments,
                type_parameters,
                segment: start.start..args_segment.end,
            }))
        } else if self
            .cursor
            .advance(spaces().then(of_type(TokenType::FatArrow)))
            .is_some()
        {
            ensure_empty(
                "Illegal expression before lambda parameter declaration",
                path,
                InclusionPathItem::segment,
            )?;
            ensure_empty(
                "Illegal expression after lambda parameter declaration",
                type_parameters,
                Type::segment,
            )?;
            let body = Box::new(self.value()?);
            let segment = name_segment.start..body.segment().end;
            Ok(Expr::LambdaDef(LambdaDef {
                args: vec![TypedVariable {
                    name,
                    ty: None,
                    segment: self.cursor.relative_pos(name),
                }],
                body,
                segment,
            }))
        } else {
            ensure_empty(
                "Command calls cannot have generic arguments",
                type_parameters,
                Type::segment,
            )?;
            ensure_empty(
                "Command calls cannot have inclusion path",
                path,
                InclusionPathItem::segment,
            )?;
            self.call_arguments(callee)
        }
    }

    fn call(&mut self) -> ParseResult<Expr<'a>> {
        let callee = self.call_argument()?;
        let type_parameters = self.parse_type_parameter_list()?.0;
        ensure_empty(
            "Command calls cannot have generic arguments",
            type_parameters,
            Type::segment,
        )?;
        self.call_arguments(callee)
    }

    fn programmatic_call(&mut self) -> ParseResult<Expr<'a>> {
        let mut path = self.parse_inclusion_path()?;
        let name = self
            .cursor
            .force(of_type(TokenType::Identifier), "Expected function name.")
            .map_err(|err| {
                err.with_kind(ParseErrorKind::Expected(
                    "valid function identifier".to_string(),
                ))
            })?;

        let name_segment = self.cursor.relative_pos(name.value);

        let type_parameters = self.parse_type_parameter_list()?.0;
        let open_parenthesis = self.cursor.force(
            of_type(TokenType::RoundedLeftBracket),
            "Expected opening parenthesis.",
        )?;

        self.delimiter_stack.push_back(open_parenthesis.clone());
        let (arguments, args_segment) = self.parse_comma_separated_arguments(open_parenthesis)?;

        let start = path
            .first()
            .map(InclusionPathItem::segment)
            .unwrap_or_else(|| name_segment.clone());

        path.push(InclusionPathItem::Symbol(name.value, name_segment));
        let segment = start.start..args_segment.end;
        Ok(Expr::ProgrammaticCall(ProgrammaticCall {
            path,
            arguments,
            type_parameters,
            segment,
        }))
    }

    fn method_call_on(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
        let dot = self.cursor.advance(of_type(TokenType::Dot));
        let name = dot
            .is_some()
            .then(|| {
                self.cursor
                    .force(of_type(TokenType::Identifier), "Expected function name.")
                    .map_err(|err| {
                        err.with_kind(ParseErrorKind::Expected(
                            "valid function identifier".to_string(),
                        ))
                    })
            })
            .transpose()?;

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

    fn call_arguments(&mut self, callee: Expr<'a>) -> ParseResult<Expr<'a>> {
        let mut arguments = vec![callee];

        while self
            .cursor
            .lookahead(spaces().then(like(TokenType::is_call_bound)))
            .is_none()
        {
            self.cursor.advance(spaces()); //consume word separations
            if self.is_at_redirection_sign() {
                return self.redirectable(Expr::Call(Call { arguments }));
            }

            arguments.push(self.call_argument()?);
        }

        Ok(Expr::Call(Call { arguments }))
    }

    fn may_be_at_programmatic_call_start(&self) -> bool {
        self.cursor
            .lookahead(any().then(of_types(&[
                TokenType::ColonColon,
                TokenType::RoundedLeftBracket,
                TokenType::SquaredLeftBracket,
            ])))
            .is_some()
    }
}

impl<'a> Parser<'a> {
    /// special pivot method for argument methods
    pub(crate) fn call_argument(&mut self) -> ParseResult<Expr<'a>> {
        self.repos("Expected value")?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            TokenType::RoundedLeftBracket => Ok(Expr::Parenthesis(self.parenthesis()?)),
            TokenType::CurlyLeftBracket => Ok(Expr::Block(self.block()?)),
            TokenType::Identifier | TokenType::Reef if self.may_be_at_programmatic_call_start() => {
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
            if let Some(comma) = self.cursor.advance(of_type(TokenType::Comma)) {
                self.report_error(self.mk_parse_error(
                    "Expected argument.",
                    comma,
                    ParseErrorKind::Unexpected,
                ));
                continue;
            }
            match self.value() {
                Ok(arg) => args.push(arg),
                Err(err) => {
                    self.recover_from(err, of_type(TokenType::Comma));
                }
            }
            self.cursor.advance(spaces());

            // Check if the arg list is abnormally terminated.
            if self.cursor.lookahead(line_end()).is_some() {
                self.expected(
                    "Expected closing parenthesis.",
                    ParseErrorKind::Unpaired(self.cursor.relative_pos(open_parenthesis.clone())),
                )?;
            }
            if self.cursor.lookahead(eog()).is_some() {
                let closing_parenthesis = self.expect_delimiter(TokenType::RoundedRightBracket)?;
                segment.end = self.cursor.relative_pos_ctx(closing_parenthesis).end;
                break;
            }
            self.cursor.force(
                spaces().then(of_type(TokenType::Comma).or(lookahead(eog()))),
                &format!("expected ',', found {}", self.cursor.peek().value),
            )?;
        }

        Ok((args, segment))
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::{Call, ProgrammaticCall};
    use ast::r#type::{ParametrizedType, Type};
    use ast::r#use::InclusionPathItem;
    use ast::value::Literal;
    use ast::Expr;
    use context::source::{Source, SourceSegmentHolder};
    use context::str_find::{find_between, find_in, find_in_nth};

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use crate::source::{literal, literal_nth};

    #[test]
    fn wrong_group_end() {
        let content = "ls )";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_next(),
            Err(ParseError {
                message: "expected end of expression or file".to_string(),
                position: content.find(')').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        );
    }

    #[test]
    fn call() {
        let source = Source::unknown("echo x y");
        assert_eq!(
            Parser::new(source).parse_next(),
            Ok(Expr::Call(Call {
                arguments: vec![
                    literal(source.source, "echo"),
                    literal(source.source, "x"),
                    literal(source.source, "y"),
                ],
            }))
        );
    }

    #[test]
    fn echo_reef() {
        let source = Source::unknown("echo reef()");
        assert_eq!(
            Parser::new(source)
                .parse_next()
                .expect_err("successful parse"),
            ParseError {
                message: "Expected function name.".to_string(),
                position: find_in(source.source, "reef"),
                kind: ParseErrorKind::Expected("valid function identifier".to_string())
            }
        );
    }

    #[test]
    fn call_with_inclusion_path() {
        let source = Source::unknown("a::b::c x y");
        assert_eq!(
            Parser::new(source).parse_next(),
            Err(ParseError {
                message: "Command calls cannot have inclusion path".to_string(),
                position: find_in(source.source, "a::b"),
                kind: ParseErrorKind::Unexpected,
            })
        );
    }

    #[test]
    fn lambdef_with_inclusion_path() {
        let source = Source::unknown("a::b => $b");
        assert_eq!(
            Parser::new(source).parse_next(),
            Err(ParseError {
                message: "Illegal expression before lambda parameter declaration".to_string(),
                position: find_in(source.source, "a"),
                kind: ParseErrorKind::Unexpected,
            })
        );
    }

    #[test]
    fn lambdef_with_type_params() {
        let source = Source::unknown("b[C] => $b");
        assert_eq!(
            Parser::new(source).parse_next(),
            Err(ParseError {
                message: "Illegal expression after lambda parameter declaration".to_string(),
                position: find_in(source.source, "C"),
                kind: ParseErrorKind::Unexpected,
            })
        );
    }
    #[test]
    fn call_with_type_parameters() {
        let source = Source::unknown("parse[Int] x y");
        assert_eq!(
            Parser::new(source).parse_next(),
            Err(ParseError {
                message: "Command calls cannot have generic arguments".to_string(),
                position: find_in(source.source, "Int"),
                kind: ParseErrorKind::Unexpected,
            })
        );
    }

    #[test]
    fn call_with_path() {
        let content = "a/b/parse x y";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_next(),
            Ok(Expr::Call(Call {
                arguments: vec![
                    literal(source.source, "a/b/parse"),
                    literal(source.source, "x"),
                    literal(source.source, "y")
                ],
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
                        literal(source.source, "grep"),
                        literal(source.source, "-E"),
                        literal(source.source, "regex")
                    ],
                }),
                Expr::Call(Call {
                    arguments: vec![
                        literal(source.source, "echo"),
                        literal(source.source, "test")
                    ],
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
                    literal(source.source, "g++"),
                    literal(source.source, "-std=c++20"),
                    literal(source.source, "-Wall"),
                    literal(source.source, "-Wextra"),
                    literal(source.source, "-Wpedantic"),
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
                    literal(source.source, "grep"),
                    literal(source.source, "-E"),
                    literal(source.source, "regex"),
                    literal(source.source, ";"),
                    literal(source.source, "echo"),
                    literal(source.source, "test"),
                ],
            }),]
        )
    }

    #[test]
    fn empty_constructor() {
        let source = Source::unknown("Foo()");
        let source2 = Source::unknown("Foo( )");
        let expr = parse(source).expect("Failed to parse");
        let expr2 = parse(source2.clone()).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(
                    "Foo",
                    find_in(source.source, "Foo")
                )],
                arguments: vec![],
                type_parameters: vec![],
                segment: source.segment(),
            })]
        );
        assert_eq!(
            expr2,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(
                    "Foo",
                    find_in(source2.source, "Foo")
                )],
                arguments: vec![],
                type_parameters: vec![],
                segment: source2.segment(),
            })]
        );
    }

    #[test]
    fn parse_constructor() {
        let source = Source::unknown("Foo('a', 2, 'c')");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(
                    "Foo",
                    find_in(source.source, "Foo")
                )],
                arguments: vec![
                    literal(source.source, "'a'"),
                    Expr::Literal(Literal {
                        parsed: 2.into(),
                        segment: find_in(source.source, "2")
                    }),
                    literal(source.source, "'c'"),
                ],
                type_parameters: vec![],
                segment: source.segment(),
            })],
        );
    }

    #[test]
    fn constructor_with_newlines_and_space() {
        let source = Source::unknown("Foo( \\\n'this' , \\\n  'is',\\\n'fine')");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(
                    "Foo",
                    find_in(source.source, "Foo")
                )],
                arguments: vec![
                    literal(source.source, "'this'"),
                    literal(source.source, "'is'"),
                    literal(source.source, "'fine'"),
                ],
                type_parameters: vec![],
                segment: source.segment(),
            })],
        );
    }

    #[test]
    fn constructor_accept_string_literals() {
        let source = Source::unknown("Foo('===\ntesting something\n===', 'c')");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(
                    "Foo",
                    find_in(source.source, "Foo")
                )],
                arguments: vec![
                    Expr::Literal(Literal {
                        parsed: "===\ntesting something\n===".into(),
                        segment: find_between(source.source, "'", "'")
                    }),
                    literal(source.source, "'c'"),
                ],
                type_parameters: vec![],
                segment: source.segment()
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
                path: vec![InclusionPathItem::Symbol(
                    "List",
                    find_in(source.source, "List")
                )],
                arguments: vec![Expr::Literal(Literal {
                    parsed: "hi".into(),
                    segment: find_in(source.source, "'hi'")
                })],
                type_parameters: vec![Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(
                        "Str",
                        find_in(source.source, "Str")
                    )],
                    params: vec![],
                    segment: find_in(source.source, "Str"),
                })],
                segment: source.segment(),
            })],
        );
    }

    #[test]
    fn pfc_within_pfc() {
        let source = Source::unknown("foo[Str](bar(), other[A]())");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(
                    "foo",
                    find_in(source.source, "foo")
                )],
                segment: source.segment(),
                arguments: vec![
                    Expr::ProgrammaticCall(ProgrammaticCall {
                        path: vec![InclusionPathItem::Symbol(
                            "bar",
                            find_in(source.source, "bar")
                        )],
                        arguments: Vec::new(),
                        type_parameters: Vec::new(),
                        segment: find_in(source.source, "bar()"),
                    }),
                    Expr::ProgrammaticCall(ProgrammaticCall {
                        path: vec![InclusionPathItem::Symbol(
                            "other",
                            find_in(source.source, "other")
                        )],
                        arguments: Vec::new(),
                        type_parameters: vec![Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol("A", find_in(source.source, "A"))],
                            params: Vec::new(),
                            segment: find_in(source.source, "A"),
                        })],
                        segment: find_in(source.source, "other[A]()"),
                    })
                ],
                type_parameters: vec![Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(
                        "Str",
                        find_in(source.source, "Str")
                    )],
                    params: vec![],
                    segment: find_in(source.source, "Str"),
                })],
            })],
        );
    }

    #[test]
    fn pfc_within_pfc_with_path() {
        let source = Source::unknown("reef::a::b::foo[Str](reef::std::bar(), foo::other[A]())");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![
                    InclusionPathItem::Reef(find_in(source.source, "reef")),
                    InclusionPathItem::Symbol("a", find_in(source.source, "a")),
                    InclusionPathItem::Symbol("b", find_in(source.source, "b")),
                    InclusionPathItem::Symbol("foo", find_in(source.source, "foo"))
                ],
                segment: source.segment(),
                arguments: vec![
                    Expr::ProgrammaticCall(ProgrammaticCall {
                        path: vec![
                            InclusionPathItem::Reef(find_in_nth(source.source, "reef", 1)),
                            InclusionPathItem::Symbol("std", find_in(source.source, "std")),
                            InclusionPathItem::Symbol("bar", find_in(source.source, "bar"))
                        ],
                        arguments: Vec::new(),
                        type_parameters: Vec::new(),
                        segment: find_in(source.source, "reef::std::bar()"),
                    }),
                    Expr::ProgrammaticCall(ProgrammaticCall {
                        path: vec![
                            InclusionPathItem::Symbol("foo", find_in_nth(source.source, "foo", 1)),
                            InclusionPathItem::Symbol("other", find_in(source.source, "other"))
                        ],
                        arguments: Vec::new(),
                        segment: find_in(source.source, "foo::other[A]()"),
                        type_parameters: vec![Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol("A", find_in(source.source, "A"))],
                            params: Vec::new(),
                            segment: find_in(source.source, "A"),
                        })]
                    })
                ],
                type_parameters: vec![Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(
                        "Str",
                        find_in(source.source, "Str")
                    )],
                    params: vec![],
                    segment: find_in(source.source, "Str"),
                })],
            })],
        );
    }

    #[test]
    fn generic_constructor_with_include_path() {
        let source = Source::unknown("foo::bar::List[Str]('hi')");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![
                    InclusionPathItem::Symbol("foo", find_in(source.source, "foo")),
                    InclusionPathItem::Symbol("bar", find_in(source.source, "bar")),
                    InclusionPathItem::Symbol("List", find_in(source.source, "List")),
                ],
                arguments: vec![Expr::Literal(Literal {
                    segment: find_in(source.source, "'hi'"),
                    parsed: "hi".into(),
                })],
                type_parameters: vec![Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(
                        "Str",
                        find_in(source.source, "Str")
                    )],
                    params: vec![],
                    segment: find_in(source.source, "Str")
                })],
                segment: source.segment(),
            })],
        );
    }

    #[test]
    fn constructor_with_unpaired_parenthesis() {
        let content = "Foo('a', 2, \"c\"\n)";
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
