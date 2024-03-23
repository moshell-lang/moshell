use ast::r#type::{ByName, CallableType, ParametrizedType, Type, TypeParameter};
use ast::variable::Identifier;
use context::display::fmt_comma_separated;
use context::source::{SourceSegment, SourceSegmentHolder};
use lexer::token::TokenType;

use crate::err::ParseErrorKind::{Expected, Unexpected};
use crate::moves::{blanks, not, of_type, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    pub(crate) fn parse_type(&mut self) -> ParseResult<Type> {
        self.cursor.advance(blanks());

        let first_token = self.cursor.peek();
        // if there's a parenthesis then the type is necessarily a lambda type
        let mut tpe = match first_token.token_type {
            TokenType::RoundedLeftBracket => self.parse_parentheses()?,
            TokenType::FatArrow => self.parse_by_name().map(Type::ByName)?,
            _ => self.parse_parametrized().map(Type::Parametrized)?,
        };
        //check if there's an arrow, if some, we are maybe in a case where the type is "A => ..."
        // (a lambda with one parameter and no parenthesis for the input, which is valid)
        if self
            .cursor
            .advance(blanks().then(of_type(TokenType::FatArrow)))
            .is_none()
        {
            return Ok(tpe);
        }

        if matches!(tpe, Type::Parametrized(..)) {
            let output = self.parse_parametrized()?;
            let segment = first_token.span.start..output.segment().end;
            tpe = Type::Callable(CallableType {
                params: vec![tpe],
                output: Box::new(Type::Parametrized(output)),
                segment,
            })
        }

        //check if there's an arrow, if some, we are maybe in a case where the type is "A => ..."
        // (a lambda with one parameter and no parenthesis for the input, which is valid)
        if self
            .cursor
            .advance(blanks().then(of_type(TokenType::FatArrow)))
            .is_some()
        {
            //parse second lambda output in order to advance on the full invalid lambda expression
            let second_rt = self.parse_parametrized()?;
            return self.expected_with(
                "Lambda type as input of another lambda must be surrounded with parenthesis",
                first_token.span.start..self.cursor.peek().span.end,
                Expected(self.unparenthesised_lambda_input_tip(tpe, Type::Parametrized(second_rt))),
            );
        }

        Ok(tpe)
    }

    pub(crate) fn parse_type_parameter(&mut self) -> ParseResult<TypeParameter> {
        let name = self.cursor.next()?;

        match name.token_type {
            TokenType::Identifier => {
                let (params, params_segment) = self.parse_optional_list(
                    TokenType::SquaredLeftBracket,
                    TokenType::SquaredRightBracket,
                    "Expected type parameter.",
                    Self::parse_type_parameter,
                )?;

                let name_segment = name.span.clone();
                let segment_start = name_segment.start;
                let segment_end = if let Some(params_segment) = params_segment {
                    params_segment.end
                } else {
                    name_segment.end
                };

                let segment = segment_start..segment_end;

                Ok(TypeParameter {
                    name: Identifier::extract(self.source, name.span),
                    params,
                    segment,
                })
            }
            x if x.is_closing_punctuation() => {
                self.expected_with("expected type", name.span, Expected("<type>".to_string()))
            }

            _ => self.expected_with(
                format!(
                    "`{}` is not a valid generic type identifier.",
                    name.text(self.source)
                ),
                name.span,
                Unexpected,
            ),
        }
    }

    fn parse_by_name(&mut self) -> ParseResult<ByName> {
        let arrow = self
            .cursor
            .force(of_type(TokenType::FatArrow), "'=>' expected here.")?;

        //control case of => => A which is invalid
        self.cursor.force(
            spaces().then(not(of_type(TokenType::FatArrow))),
            "unexpected '=>'",
        )?;

        let name = self.parse_type().map(Box::new)?;
        let segment = arrow.span.start..name.segment().end;
        Ok(ByName { name, segment })
    }

    fn unparenthesised_lambda_input_tip(&self, left_lambda: Type, lambda_out: Type) -> String {
        "(".to_string() + &left_lambda.to_string() + ") => " + &lambda_out.to_string()
    }

    fn parse_parentheses(&mut self) -> ParseResult<Type> {
        let (inputs, segment) = self.parse_implicit_list(
            TokenType::RoundedLeftBracket,
            TokenType::RoundedRightBracket,
            "Expected type.",
            Self::parse_type,
        )?;

        //if there is an arrow then it is a lambda
        if self
            .cursor
            .advance(spaces().then(of_type(TokenType::FatArrow)))
            .is_some()
        {
            return self
                .parse_lambda_with_inputs(segment, inputs)
                .map(Type::Callable);
        }

        //its a type of form `(A)`
        if let Some(ty) = inputs.first() {
            if inputs.len() == 1 {
                return Ok(ty.clone());
            }
        }

        let mut rendered_tuple = String::new();
        fmt_comma_separated('(', ')', &inputs, &mut rendered_tuple).unwrap();

        rendered_tuple += " => <types>";
        self.expected(
            "Tuples are not yet supported. A lambda declaration was expected here",
            Expected(rendered_tuple),
        )
    }

    fn parse_lambda_with_inputs(
        &mut self,
        inputs_segment: SourceSegment,
        inputs: Vec<Type>,
    ) -> ParseResult<CallableType> {
        let output = Box::new(self.parse_type()?);
        let segment = inputs_segment.start..output.segment().end;
        Ok(CallableType {
            params: inputs,
            output,
            segment,
        })
    }

    fn parse_parametrized(&mut self) -> ParseResult<ParametrizedType> {
        self.cursor.advance(spaces());
        if !matches!(
            self.cursor.peek().token_type,
            TokenType::Identifier | TokenType::Reef
        ) {
            return self.expected(
                format!(
                    "`{}` is not a valid type identifier.",
                    self.cursor.peek().text(self.source)
                ),
                Unexpected,
            );
        }
        let path = self.parse_path()?;
        let mut segment = path.segment();

        let (params, params_segment) = self.parse_optional_list(
            TokenType::SquaredLeftBracket,
            TokenType::SquaredRightBracket,
            "Expected type.",
            Self::parse_type,
        )?;
        if let Some(params_segment) = params_segment {
            segment.end = params_segment.end;
        }
        Ok(ParametrizedType {
            path: path.path,
            params,
            segment,
        })
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::r#type::{ByName, CallableType, ParametrizedType, Type};
    use ast::r#use::InclusionPathItem;
    use context::source::SourceSegmentHolder;
    use context::str_find::find_in;

    use crate::err::ParseError;
    use crate::err::ParseErrorKind::{Expected, Unexpected};
    use crate::parser::Parser;
    use crate::source::identifier;

    #[test]
    fn simple_type() {
        let source = "MyType";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Parametrized(ParametrizedType {
                path: vec![InclusionPathItem::Symbol(identifier(source, "MyType"))],
                params: Vec::new(),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn simple_type_include_path() {
        let source = "reef::std::MyType";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Parametrized(ParametrizedType {
                path: vec![
                    InclusionPathItem::Reef(find_in(source, "reef")),
                    InclusionPathItem::Symbol(identifier(source, "std")),
                    InclusionPathItem::Symbol(identifier(source, "MyType")),
                ],
                params: Vec::new(),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn empty_param_list() {
        let source = "Complex[    ]";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Parametrized(ParametrizedType {
                path: vec![InclusionPathItem::Symbol(identifier(source, "Complex"))],
                params: vec![],
                segment: source.segment()
            }))
        );
    }

    #[test]
    fn parametrized_types() {
        let source = "MyType[A[X, Y[Any], foo::Z], B[std::C[D]]]";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Parametrized(ParametrizedType {
                path: vec![InclusionPathItem::Symbol(identifier(source, "MyType"))],
                params: vec![
                    Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "A"))],
                        params: vec![
                            Type::Parametrized(ParametrizedType {
                                path: vec![InclusionPathItem::Symbol(identifier(source, "X"))],
                                params: Vec::new(),
                                segment: find_in(source, "X"),
                            }),
                            Type::Parametrized(ParametrizedType {
                                path: vec![InclusionPathItem::Symbol(identifier(source, "Y"))],
                                params: vec![Type::Parametrized(ParametrizedType {
                                    path: vec![InclusionPathItem::Symbol(identifier(
                                        source, "Any"
                                    ))],
                                    params: Vec::new(),
                                    segment: find_in(source, "Any"),
                                })],
                                segment: find_in(source, "Y[Any]"),
                            }),
                            Type::Parametrized(ParametrizedType {
                                path: vec![
                                    InclusionPathItem::Symbol(identifier(source, "foo")),
                                    InclusionPathItem::Symbol(identifier(source, "Z"))
                                ],
                                params: Vec::new(),
                                segment: find_in(source, "foo::Z"),
                            }),
                        ],
                        segment: find_in(source, "A[X, Y[Any], foo::Z]"),
                    }),
                    Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "B"))],
                        params: vec![Type::Parametrized(ParametrizedType {
                            path: vec![
                                InclusionPathItem::Symbol(identifier(source, "std")),
                                InclusionPathItem::Symbol(identifier(source, "C"))
                            ],
                            params: vec![Type::Parametrized(ParametrizedType {
                                path: vec![InclusionPathItem::Symbol(identifier(source, "D"))],
                                params: Vec::new(),
                                segment: find_in(source, "D"),
                            })],
                            segment: find_in(source, "std::C[D]"),
                        })],
                        segment: find_in(source, "B[std::C[D]]"),
                    }),
                ],
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn type_params_missing_comma() {
        let source = "MyType[X Y]";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Err(ParseError {
                message: "A comma or a closing bracket was expected here".to_string(),
                position: "MyType[X ".len().."MyType[X ".len() + 1,
                kind: Expected("',' or ']'".to_string()),
            })
        );
    }

    #[test]
    fn type_invalid_name() {
        let source = "Complex[  @  ]";
        let res = Parser::new(source).parse_specific(Parser::parse_type);
        assert_eq!(
            res,
            Err(ParseError {
                message: "`@` is not a valid type identifier.".to_string(),
                position: source.find('@').map(|i| i..i + 1).unwrap(),
                kind: Unexpected,
            })
        );
    }

    #[test]
    fn simple_lambda_declaration() {
        let source = "A => B";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Callable(CallableType {
                params: vec![Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "A"))],
                    params: Vec::new(),
                    segment: find_in(source, "A"),
                })],
                output: Box::new(Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "B"))],
                    params: Vec::new(),
                    segment: find_in(source, "B"),
                })),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn by_name_declaration() {
        let source = " => B";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::ByName(ByName {
                name: Box::new(Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "B"))],
                    params: Vec::new(),
                    segment: find_in(source, "B"),
                })),
                segment: 1..source.len(),
            }))
        );
    }

    #[test]
    fn nested_by_name_declaration() {
        let source = "=> => B";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Err(ParseError {
                message: "unexpected '=>'".to_string(),
                position: 3..5,
                kind: Unexpected,
            })
        );
    }

    #[test]
    fn nested_by_name_declaration_parenthesised() {
        let source = "=> (=> B)";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::ByName(ByName {
                name: Box::new(Type::ByName(ByName {
                    name: Box::new(Type::Parametrized(ParametrizedType {
                        path: vec![
                            InclusionPathItem::Symbol(identifier(source, "B")),
                        ],
                        params: Vec::new(),
                        segment: find_in(source, "B"),
                    })),
                    segment: find_in(source, "=> B"),
                })),
                segment: /*source.segment()*/0..source.len() - 1,
            }))
        );
    }

    #[test]
    fn parentheses() {
        let source = "((((A))))";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Parametrized(ParametrizedType {
                path: vec![InclusionPathItem::Symbol(identifier(source, "A"))],
                params: Vec::new(),
                segment: find_in(source, "A"),
            }))
        );
    }

    #[test]
    fn lambda_declaration_no_input() {
        let source = "() => B";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Callable(CallableType {
                params: vec![],
                output: Box::new(Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "B"))],
                    params: Vec::new(),
                    segment: find_in(source, "B"),
                })),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn lambda_declaration_void_output() {
        let source = "A => Unit";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Callable(CallableType {
                params: vec![Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "A"))],
                    params: Vec::new(),
                    segment: find_in(source, "A"),
                })],
                output: Box::new(Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "Unit"))],
                    params: Vec::new(),
                    segment: find_in(source, "Unit"),
                })),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn lambda_multiple_inputs() {
        let source = "(A, B, C) => D";
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Callable(CallableType {
                params: vec![
                    Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "A"))],
                        params: Vec::new(),
                        segment: find_in(source, "A"),
                    }),
                    Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "B"))],
                        params: Vec::new(),
                        segment: find_in(source, "B"),
                    }),
                    Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "C"))],
                        params: Vec::new(),
                        segment: find_in(source, "C"),
                    }),
                ],
                output: Box::new(Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "D"))],
                    params: Vec::new(),
                    segment: find_in(source, "D"),
                })),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn chained_lambdas() {
        let source = "((A => B) => C) => D";
        let ast = Parser::new(source).parse_specific(Parser::parse_type);
        assert_eq!(
            ast,
            Ok(Type::Callable(CallableType {
                params: vec![Type::Callable(CallableType {
                    params: vec![Type::Callable(CallableType {
                        params: vec![Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier(source, "A"))],
                            params: Vec::new(),
                            segment: find_in(source, "A"),
                        })],
                        output: Box::new(Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier(source, "B"))],
                            params: Vec::new(),
                            segment: find_in(source, "B"),
                        })),
                        segment: find_in(source, "A => B"),
                    })],
                    output: Box::new(Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "C"))],
                        params: Vec::new(),
                        segment: find_in(source, "C"),
                    })),
                    segment: find_in(source, "(A => B) => C"),
                })],
                output: Box::new(Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "D"))],
                    params: Vec::new(),
                    segment: find_in(source, "D"),
                })),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn tuple_declaration() {
        let source = "(A, B, C)";
        let ast = Parser::new(source).parse_specific(Parser::parse_type);
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Tuples are not yet supported. A lambda declaration was expected here"
                    .to_string(),
                position: source.len()..source.len(),
                kind: Expected("(A, B, C) => <types>".to_string()),
            })
        );
    }

    #[test]
    fn parenthesised_lambda_input() {
        let source = "(A, B, C) => D => E";
        let ast = Parser::new(source).parse_specific(Parser::parse_type);
        assert_eq!(
            ast,
            Ok(Type::Callable(CallableType {
                params: vec![
                    Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "A"))],
                        params: Vec::new(),
                        segment: find_in(source, "A")
                    }),
                    Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "B"))],
                        params: Vec::new(),
                        segment: find_in(source, "B")
                    }),
                    Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "C"))],
                        params: Vec::new(),
                        segment: find_in(source, "C")
                    }),
                ],
                output: Box::new(Type::Callable(CallableType {
                    params: vec![Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "D"))],
                        params: Vec::new(),
                        segment: find_in(source, "D")
                    })],
                    output: Box::new(Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "E"))],
                        params: Vec::new(),
                        segment: find_in(source, "E")
                    })),
                    segment: find_in(source, "D => E")
                })),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn unparenthesised_lambda_input() {
        let ast1 = Parser::new("(A, B, C) => D => E => F").parse_type();
        let ast2 = Parser::new("A => B => C").parse_type();
        let expected1 = Err(ParseError {
            message: "Lambda type as input of another lambda must be surrounded with parenthesis"
                .to_string(),
            kind: Expected("(D => E) => F".to_string()),
            position: 13..24,
        });
        let expected2 = Err(ParseError {
            message: "Lambda type as input of another lambda must be surrounded with parenthesis"
                .to_string(),
            kind: Expected("(A => B) => C".to_string()),
            position: 0..11,
        });
        assert_eq!(ast1, expected1);
        assert_eq!(ast2, expected2);
    }
}
