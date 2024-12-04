use ast::call::{Call, MethodCall, ProgrammaticCall};
use ast::r#struct::FieldAccess;
use ast::r#type::Type;
use ast::variable::{Identifier, Path};
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};
use lexer::token::{Token, TokenType};

use crate::aspects::literal::LiteralLeniency;
use crate::err::ParseErrorKind;
use crate::moves::{
    any, blanks, eog, identifier_parenthesis, like, line_end, lookahead, of_type, of_types, spaces,
    Move,
};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    /// Parses a shell call, delimited by spaces.
    pub(crate) fn call(&mut self) -> ParseResult<Expr> {
        let callee = self.call_argument()?;
        self.call_arguments(callee)
    }

    /// Parses a programmatic call, delimited by parentheses.
    pub(crate) fn programmatic_call(
        &mut self,
        path: Path,
        type_parameters: Vec<Type>,
    ) -> ParseResult<Expr> {
        let open_parenthesis = self.cursor.force(
            of_type(TokenType::RoundedLeftBracket),
            "Expected opening parenthesis.",
        )?;

        let (arguments, args_segment) = self.parse_comma_separated_arguments(open_parenthesis)?;

        let start = path.segment();

        let segment = start.start..args_segment.end;

        Ok(Expr::ProgrammaticCall(ProgrammaticCall {
            path: path.path,
            arguments,
            type_parameters,
            segment,
        }))
    }

    /// Parses a method call on a callee expression.
    fn method_call_on(&mut self, expr: Expr) -> ParseResult<Expr> {
        self.cursor.advance(of_type(TokenType::ColonColon));
        let (type_arguments, _) = self.parse_optional_list(
            TokenType::SquaredLeftBracket,
            TokenType::SquaredRightBracket,
            "expected type argument",
            Parser::parse_type,
        )?;

        let open_parenthesis = self.cursor.force(
            of_type(TokenType::RoundedLeftBracket),
            "Expected opening parenthesis.",
        )?;
        let (arguments, segment) = self.parse_comma_separated_arguments(open_parenthesis)?;
        let segment = expr.segment().start..segment.end;
        Ok(match expr {
            Expr::FieldAccess(FieldAccess { expr, field, .. }) => Expr::MethodCall(MethodCall {
                source: expr,
                name: Some(field),
                arguments,
                type_parameters: type_arguments,
                segment,
            }),
            expr => Expr::MethodCall(MethodCall {
                source: Box::new(expr),
                name: None,
                arguments,
                type_parameters: type_arguments,
                segment,
            }),
        })
    }

    /// Parse any function call or method call after an expression.
    pub(crate) fn expand_member_chain(&mut self, mut expr: Expr) -> ParseResult<Expr> {
        loop {
            let pivot = self
                .cursor
                .lookahead(spaces().then(any()))
                .unwrap()
                .token_type;

            expr = match pivot {
                TokenType::SquaredLeftBracket => {
                    self.cursor.advance(blanks());
                    self.parse_subscript(expr).map(Expr::Subscript)?
                }
                TokenType::ColonColon | TokenType::RoundedLeftBracket => match expr {
                    Expr::Path(ident) => self.programmatic_call(ident, Vec::new())?,
                    expr => self.method_call_on(expr)?,
                },
                _ if self
                    .cursor
                    .lookahead(of_type(TokenType::Dot).or(
                        blanks().then(of_type(TokenType::Dot).and_then(identifier_parenthesis())),
                    ))
                    .is_some() =>
                {
                    self.cursor.advance(blanks());
                    let dot_token = self.cursor.next()?;
                    let field_token = self.cursor.force(
                        of_type(TokenType::Identifier),
                        "identifier expected when accessing attribute",
                    )?;
                    let segment = dot_token.span.start..field_token.span.end;
                    Expr::FieldAccess(FieldAccess {
                        expr: Box::new(expr),
                        field: Identifier::extract(self.source, field_token.span),
                        segment,
                    })
                }
                _ => break,
            };
        }
        Ok(expr)
    }

    /// Continues to parse a call expression from a known command name expression.
    pub(crate) fn call_arguments(&mut self, callee: Expr) -> ParseResult<Expr> {
        let mut arguments = vec![callee];

        while self
            .cursor
            .lookahead(spaces().then(like(TokenType::is_call_bound)))
            .is_none()
        {
            self.cursor.advance(spaces()); //consume word separations
            if self
                .cursor
                .lookahead(
                    of_types(&[TokenType::Less, TokenType::Greater])
                        .and_then(of_type(TokenType::RoundedLeftBracket)),
                )
                .is_some()
            {
                let direction_token = self.cursor.next()?;
                arguments.push(self.process_substitution(direction_token)?);
                continue;
            }
            if self.is_at_redirection_sign() {
                return self.redirectable(Expr::Call(Call { arguments }));
            }

            arguments.push(self.call_argument()?);
        }

        Ok(Expr::Call(Call { arguments }))
    }

    /// special pivot method for argument methods
    pub(crate) fn call_argument(&mut self) -> ParseResult<Expr> {
        self.repos("Expected value")?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            TokenType::RoundedLeftBracket => Ok(Expr::Parenthesis(self.parenthesis()?)),
            TokenType::CurlyLeftBracket => Ok(Expr::Block(self.block()?)),
            TokenType::Less | TokenType::Greater => {
                let redir = self.cursor.next()?;
                self.process_substitution(redir)
            }
            TokenType::Identifier | TokenType::Reef if self.is_path() => {
                let path = self.parse_path()?;
                self.expand_member_chain(Expr::Path(path))
            }
            _ => self.literal(LiteralLeniency::Lenient),
        }
    }

    fn parse_comma_separated_arguments(
        &mut self,
        open_parenthesis: Token,
    ) -> ParseResult<(Vec<Expr>, SourceSegment)> {
        // Read the args until a closing delimiter or a new non-escaped line is found.
        let mut args = Vec::new();
        let mut segment = open_parenthesis.span.clone();
        loop {
            self.cursor.advance(spaces());
            if let Some(closing_parenthesis) =
                self.cursor.advance(of_type(TokenType::RoundedRightBracket))
            {
                segment.end = closing_parenthesis.span.end;
                return Ok((args, segment));
            }
            if let Some(comma) = self.cursor.advance(of_type(TokenType::Comma)) {
                self.report_error(self.mk_parse_error(
                    "Expected argument.",
                    comma.span,
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
                    ParseErrorKind::Unpaired(open_parenthesis.span.clone()),
                )?;
            }
            if self.cursor.lookahead(eog()).is_some() {
                let closing_parenthesis =
                    self.expect_delimiter(open_parenthesis, TokenType::RoundedRightBracket)?;
                segment.end = closing_parenthesis.span.end;
                break;
            }
            self.cursor.force(
                spaces().then(of_type(TokenType::Comma).or(lookahead(eog()))),
                &format!(
                    "expected ',', found {}",
                    self.cursor.peek().text(self.source)
                ),
            )?;
        }

        Ok((args, segment))
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::{Call, MethodCall, ProgrammaticCall};
    use ast::group::Block;
    use ast::r#struct::FieldAccess;
    use ast::r#type::{ParametrizedType, Type};
    use ast::r#use::InclusionPathItem;
    use ast::value::Literal;
    use ast::variable::{VarName, VarReference};
    use ast::Expr;
    use context::source::SourceSegmentHolder;
    use context::str_find::{find_between, find_in, find_in_nth};

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use crate::source::{identifier, identifier_nth, literal, literal_nth};

    #[test]
    fn wrong_group_end() {
        let source = "ls )";
        assert_eq!(
            ParseResult::<Vec<Expr>>::from(parse(source)),
            Err(ParseError {
                message: "Unexpected closing delimiter.".to_string(),
                position: source.find(')').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        );
    }

    #[test]
    fn call() {
        let source = "echo x y";
        assert_eq!(
            Parser::new(source).parse_next(),
            Ok(Expr::Call(Call {
                arguments: vec![
                    literal(source, "echo"),
                    literal(source, "x"),
                    literal(source, "y"),
                ],
            }))
        );
    }

    #[test]
    fn call_with_path() {
        let source = "a/b/parse x y";
        assert_eq!(
            Parser::new(source).parse_next(),
            Ok(Expr::Call(Call {
                arguments: vec![
                    literal(source, "a/b/parse"),
                    literal(source, "x"),
                    literal(source, "y")
                ],
            }))
        );
    }

    #[test]
    fn call_space() {
        let source = r"a\ b c\ d";
        assert_eq!(
            Parser::new(source).parse_next(),
            Ok(Expr::Call(Call {
                arguments: vec![
                    Expr::Literal(Literal {
                        parsed: "a b".into(),
                        segment: find_in(source, "a\\ b")
                    }),
                    Expr::Literal(Literal {
                        parsed: "c d".into(),
                        segment: find_in(source, "c\\ d")
                    })
                ],
            }))
        );
    }

    #[test]
    fn not_in_call_is_literal() {
        let source = "echo how ! how are you !";
        let result = parse(source).expect("Failed to parse");
        assert_eq!(
            result,
            vec![Expr::Call(Call {
                arguments: vec![
                    literal(source, "echo"),
                    literal(source, "how"),
                    literal(source, "!"),
                    literal_nth(source, "how", 1),
                    literal(source, "are"),
                    literal(source, "you"),
                    literal_nth(source, "!", 1),
                ],
            })]
        );
    }

    #[test]
    fn multiple_calls() {
        let source = "grep -E regex; echo test";
        let parsed = parse(source).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![
                Expr::Call(Call {
                    arguments: vec![
                        literal(source, "grep"),
                        literal(source, "-E"),
                        literal(source, "regex")
                    ],
                }),
                Expr::Call(Call {
                    arguments: vec![literal(source, "echo"), literal(source, "test")],
                }),
            ]
        )
    }

    #[test]
    fn multiline_call() {
        let source = "g++ -std=c++20 \\\n-Wall \\\n-Wextra\\\n-Wpedantic";
        let parsed = parse(source).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![Expr::Call(Call {
                arguments: vec![
                    literal(source, "g++"),
                    literal(source, "-std=c++20"),
                    literal(source, "-Wall"),
                    literal(source, "-Wextra"),
                    literal(source, "-Wpedantic"),
                ],
            })]
        )
    }

    #[test]
    fn escaped_call() {
        let source = "grep -E regex \\; echo test";
        let parsed = parse(source).expect("Failed to parse");
        assert_eq!(
            parsed,
            vec![Expr::Call(Call {
                arguments: vec![
                    literal(source, "grep"),
                    literal(source, "-E"),
                    literal(source, "regex"),
                    literal(source, ";"),
                    literal(source, "echo"),
                    literal(source, "test"),
                ],
            })]
        )
    }

    #[test]
    fn empty_constructor() {
        let source = "Foo()";
        let source2 = "Foo( )";
        let expr = parse(source).expect("Failed to parse");
        let expr2 = parse(source2).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(identifier(source, "Foo"))],
                arguments: vec![],
                type_parameters: vec![],
                segment: source.segment(),
            })]
        );
        assert_eq!(
            expr2,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(identifier(source2, "Foo"))],
                arguments: vec![],
                type_parameters: vec![],
                segment: source2.segment(),
            })]
        );
    }

    #[test]
    fn parse_constructor() {
        let source = "Foo('a', 2, 'c')";
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(identifier(source, "Foo"))],
                arguments: vec![
                    literal(source, "'a'"),
                    Expr::Literal(Literal {
                        parsed: 2.into(),
                        segment: find_in(source, "2")
                    }),
                    literal(source, "'c'"),
                ],
                type_parameters: vec![],
                segment: source.segment(),
            })],
        );
    }

    #[test]
    fn constructor_with_newlines_and_space() {
        let source = "Foo( \\\n'this' , \\\n  'is',\\\n'fine')";
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(identifier(source, "Foo"))],
                arguments: vec![
                    literal(source, "'this'"),
                    literal(source, "'is'"),
                    literal(source, "'fine'"),
                ],
                type_parameters: vec![],
                segment: source.segment(),
            })],
        );
    }

    #[test]
    fn constructor_accept_string_literals() {
        let source = "Foo('===\ntesting something\n===', 'c')";
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(identifier(source, "Foo"))],
                arguments: vec![
                    Expr::Literal(Literal {
                        parsed: "===\ntesting something\n===".into(),
                        segment: find_between(source, "'", "'")
                    }),
                    literal(source, "'c'"),
                ],
                type_parameters: vec![],
                segment: source.segment()
            })]
        );
    }

    #[test]
    fn generic_constructor() {
        let source = "List[Str]('hi')";
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(identifier(source, "List"))],
                arguments: vec![Expr::Literal(Literal {
                    parsed: "hi".into(),
                    segment: find_in(source, "'hi'")
                })],
                type_parameters: vec![Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "Str"))],
                    params: vec![],
                    segment: find_in(source, "Str"),
                })],
                segment: source.segment(),
            })],
        );
    }

    #[test]
    fn pfc_within_pfc() {
        let source = "foo[Str](bar(), other[A]())";
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(identifier(source, "foo"))],
                segment: source.segment(),
                arguments: vec![
                    Expr::ProgrammaticCall(ProgrammaticCall {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "bar"))],
                        arguments: Vec::new(),
                        type_parameters: Vec::new(),
                        segment: find_in(source, "bar()"),
                    }),
                    Expr::ProgrammaticCall(ProgrammaticCall {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "other"))],
                        arguments: Vec::new(),
                        type_parameters: vec![Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier(source, "A"))],
                            params: Vec::new(),
                            segment: find_in(source, "A"),
                        })],
                        segment: find_in(source, "other[A]()"),
                    })
                ],
                type_parameters: vec![Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "Str"))],
                    params: vec![],
                    segment: find_in(source, "Str"),
                })],
            })],
        );
    }

    #[test]
    fn pfc_within_pfc_with_path() {
        let source = "reef::a::b::foo[Str](reef::std::bar(), foo::other[A]())";
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![
                    InclusionPathItem::Reef(find_in(source, "reef")),
                    InclusionPathItem::Symbol(identifier(source, "a")),
                    InclusionPathItem::Symbol(identifier(source, "b")),
                    InclusionPathItem::Symbol(identifier(source, "foo")),
                ],
                segment: source.segment(),
                arguments: vec![
                    Expr::ProgrammaticCall(ProgrammaticCall {
                        path: vec![
                            InclusionPathItem::Reef(find_in_nth(source, "reef", 1)),
                            InclusionPathItem::Symbol(identifier(source, "std")),
                            InclusionPathItem::Symbol(identifier(source, "bar")),
                        ],
                        arguments: Vec::new(),
                        type_parameters: Vec::new(),
                        segment: find_in(source, "reef::std::bar()"),
                    }),
                    Expr::ProgrammaticCall(ProgrammaticCall {
                        path: vec![
                            InclusionPathItem::Symbol(identifier_nth(source, "foo", 1)),
                            InclusionPathItem::Symbol(identifier(source, "other")),
                        ],
                        arguments: Vec::new(),
                        segment: find_in(source, "foo::other[A]()"),
                        type_parameters: vec![Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier(source, "A"))],
                            params: Vec::new(),
                            segment: find_in(source, "A"),
                        })]
                    })
                ],
                type_parameters: vec![Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "Str"))],
                    params: vec![],
                    segment: find_in(source, "Str"),
                })],
            })],
        );
    }

    #[test]
    fn generic_constructor_with_include_path() {
        let source = "foo::bar::List[Str]('hi')";
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![
                    InclusionPathItem::Symbol(identifier(source, "foo")),
                    InclusionPathItem::Symbol(identifier(source, "bar")),
                    InclusionPathItem::Symbol(identifier(source, "List")),
                ],
                arguments: vec![Expr::Literal(Literal {
                    segment: find_in(source, "'hi'"),
                    parsed: "hi".into(),
                })],
                type_parameters: vec![Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "Str"))],
                    params: vec![],
                    segment: find_in(source, "Str")
                })],
                segment: source.segment(),
            })],
        );
    }

    #[test]
    fn constructor_with_unpaired_parenthesis() {
        let source = "Foo('a', 2, \"c\"\n)";
        let expr: ParseResult<_> = parse(source).into();
        assert_eq!(
            expr,
            Err(ParseError {
                message: "Expected closing parenthesis.".into(),
                position: source.find('\n').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unpaired(source.find('(').map(|p| p..p + 1).unwrap())
            })
        )
    }

    #[test]
    fn constructor_exit_when_mismatched_bracket() {
        let source = "Foo(41 ]";
        let expr: ParseResult<_> = parse(source).into();
        assert_eq!(
            expr,
            Err(ParseError {
                message: "Mismatched closing delimiter.".into(),
                position: source.len() - 1..source.len(),
                kind: ParseErrorKind::Unpaired(source.find('(').map(|p| p..p + 1).unwrap())
            })
        )
    }

    #[test]
    fn field_access() {
        let source = "{$self.b.method().field}.other_field";
        let expr: ParseResult<_> = parse(source).into();
        assert_eq!(
            expr,
            Ok(vec![Expr::FieldAccess(FieldAccess {
                expr: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::FieldAccess(FieldAccess {
                        expr: Box::new(Expr::MethodCall(MethodCall {
                            source: Box::new(Expr::FieldAccess(FieldAccess {
                                expr: Box::new(Expr::VarReference(VarReference {
                                    name: VarName::Slf,
                                    segment: find_in(source, "$self"),
                                })),
                                field: identifier(source, "b"),
                                segment: find_in(source, ".b"),
                            })),
                            name: Some(identifier(source, "method")),
                            arguments: vec![],
                            type_parameters: vec![],
                            segment: find_in(source, ".method()"),
                        })),
                        field: identifier(source, "field"),
                        segment: find_in(source, ".field"),
                    })],
                    segment: find_in(source, "{$self.b.method().field}"),
                })),
                field: identifier(source, "other_field"),
                segment: find_in(source, ".other_field"),
            })])
        )
    }
}
