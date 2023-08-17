use ast::r#type::{ByName, CallableType, CastedExpr, ParametrizedType, Type};
use ast::Expr;
use context::display::fmt_comma_separated;
use context::source::{SourceSegment, SourceSegmentHolder};
use lexer::token::TokenType::*;
use lexer::token::TokenType::{
    FatArrow, Identifier, NewLine, RoundedLeftBracket, RoundedRightBracket, SquaredLeftBracket,
    SquaredRightBracket,
};

use crate::aspects::expr_list::ExpressionListAspect;
use crate::aspects::modules::ModulesAspect;
use crate::err::ParseErrorKind::{Expected, Unexpected};
use crate::moves::{blanks, not, of_type, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};

///A parser aspect to parse all type declarations, such as lambdas, constant types, parametrized type and Unit
pub trait TypeAspect<'a> {
    ///parse a lambda type signature, a constant or parametrized type or Unit.
    fn parse_type(&mut self) -> ParseResult<Type<'a>>;

    ///parse a type parameter list (`[...]`)
    fn parse_type_parameter_list(&mut self) -> ParseResult<(Vec<Type<'a>>, SourceSegment)>;

    ///parse a casted expression (ex: {..} as Type)
    fn parse_cast(&mut self, casted_expr: Expr<'a>) -> ParseResult<CastedExpr<'a>>;
}

impl<'a> TypeAspect<'a> for Parser<'a> {
    fn parse_type(&mut self) -> ParseResult<Type<'a>> {
        self.cursor.advance(blanks());

        let first_token = self.cursor.peek();
        // if there's a parenthesis then the type is necessarily a lambda type
        let mut tpe = match first_token.token_type {
            RoundedLeftBracket => self.parse_parentheses()?,
            FatArrow => self.parse_by_name().map(Type::ByName)?,
            _ => self.parse_parametrized()?,
        };
        //check if there's an arrow, if some, we are maybe in a case where the type is "A => ..."
        // (a lambda with one parameter and no parenthesis for the input, which is valid)
        if self
            .cursor
            .advance(blanks().then(of_type(FatArrow)))
            .is_none()
        {
            return Ok(tpe);
        }

        if matches!(tpe, Type::Parametrized(..)) {
            let output = self.parse_parametrized()?;
            let segment =
                self.cursor.relative_pos_ctx(first_token.clone()).start..output.segment().end;
            tpe = Type::Callable(CallableType {
                params: vec![tpe],
                output: Box::new(output),
                segment,
            })
        }

        //check if there's an arrow, if some, we are maybe in a case where the type is "A => ..."
        // (a lambda with one parameter and no parenthesis for the input, which is valid)
        if self
            .cursor
            .advance(blanks().then(of_type(FatArrow)))
            .is_some()
        {
            //parse second lambda output in order to advance on the full invalid lambda expression
            let second_rt = self.parse_parametrized()?;
            return self.expected_with(
                "Lambda type as input of another lambda must be surrounded with parenthesis",
                first_token..self.cursor.peek(),
                Expected(self.unparenthesised_lambda_input_tip(tpe, second_rt)),
            );
        }

        Ok(tpe)
    }

    fn parse_type_parameter_list(&mut self) -> ParseResult<(Vec<Type<'a>>, SourceSegment)> {
        if self.cursor.lookahead(of_type(SquaredLeftBracket)).is_none() {
            return Ok((Vec::new(), self.cursor.relative_pos_ctx(self.cursor.peek())));
        }
        let start = self.cursor.peek();
        let ((tparams, segment), no_nested_errors) = self.observe_error_reports(|p| {
            p.parse_explicit_list(SquaredLeftBracket, SquaredRightBracket, Self::parse_type)
        })?;
        if no_nested_errors && tparams.is_empty() {
            return self.expected_with(
                "unexpected empty type parameter list",
                start..self.cursor.peek(),
                Unexpected,
            );
        }
        Ok((tparams, segment))
    }

    fn parse_cast(&mut self, casted_expr: Expr<'a>) -> ParseResult<CastedExpr<'a>> {
        self.cursor.force(
            blanks().then(of_type(As)),
            "expected 'as' for cast expression.",
        )?;
        let casted_type = self.parse_type()?;
        let segment = casted_expr.segment().start..casted_type.segment().end;
        Ok(CastedExpr {
            expr: Box::new(casted_expr),
            casted_type,
            segment,
        })
    }
}

impl<'a> Parser<'a> {
    fn parse_by_name(&mut self) -> ParseResult<ByName<'a>> {
        let arrow = self
            .cursor
            .force(of_type(FatArrow), "'=>' expected here.")?;

        //control case of => => A which is invalid
        self.cursor
            .force(spaces().then(not(of_type(FatArrow))), "unexpected '=>'")?;

        let name = self.parse_type().map(Box::new)?;
        let segment = self.cursor.relative_pos_ctx(arrow).start..name.segment().end;
        Ok(ByName { name, segment })
    }

    fn unparenthesised_lambda_input_tip(
        &self,
        left_lambda: Type<'a>,
        lambda_out: Type<'a>,
    ) -> String {
        "(".to_string() + &left_lambda.to_string() + ") => " + &lambda_out.to_string()
    }

    fn parse_parentheses(&mut self) -> ParseResult<Type<'a>> {
        let (inputs, segment) =
            self.parse_implicit_list(RoundedLeftBracket, RoundedRightBracket, Self::parse_type)?;

        //if there is an arrow then it is a lambda
        if self
            .cursor
            .advance(spaces().then(of_type(FatArrow)))
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
        inputs: Vec<Type<'a>>,
    ) -> ParseResult<CallableType<'a>> {
        let output = Box::new(self.parse_type()?);
        let segment = inputs_segment.start..output.segment().end;
        Ok(CallableType {
            params: inputs,
            output,
            segment,
        })
    }

    fn parse_parametrized(&mut self) -> ParseResult<Type<'a>> {
        self.cursor.advance(spaces());
        let start = self.cursor.peek();
        let path = self.parse_inclusion_path()?;
        self.cursor.advance(spaces());
        let name_token = self.cursor.peek();
        let mut segment = self.cursor.relative_pos_ctx(start..name_token.clone());

        return match name_token.token_type {
            Identifier => {
                self.cursor.next_opt();
                let (params, params_segment) = self.parse_type_parameter_list()?;
                if !params.is_empty() {
                    segment.end = params_segment.end;
                }
                Ok(Type::Parametrized(ParametrizedType {
                    path,
                    name: name_token.value,
                    params,
                    segment,
                }))
            }

            NewLine => self.expected_with(
                "expected types",
                name_token,
                Expected("<types>".to_string()),
            ),

            x if x.is_closing_ponctuation() => self.expected_with(
                "expected types",
                name_token,
                Expected("<types>".to_string()),
            ),

            _ => self.expected_with(
                format!("'{}' is not a valid type identifier.", name_token.value),
                name_token.value,
                Unexpected,
            ),
        };
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::r#type::{ByName, CallableType, ParametrizedType, Type};
    use ast::r#use::InclusionPathItem;
    use context::source::{Source, SourceSegmentHolder};
    use context::str_find::find_in;

    use crate::aspects::r#type::TypeAspect;
    use crate::err::ParseError;
    use crate::err::ParseErrorKind::{Expected, Unexpected, Unpaired};
    use crate::parser::Parser;

    #[test]
    fn simple_type() {
        let content = "MyType";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Parametrized(ParametrizedType {
                path: vec![],
                name: "MyType",
                params: Vec::new(),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn simple_type_include_path() {
        let content = "reef::std::MyType";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Parametrized(ParametrizedType {
                path: vec![
                    InclusionPathItem::Reef(find_in(content, "reef")),
                    InclusionPathItem::Symbol("std", find_in(content, "std")),
                ],
                name: "MyType",
                params: Vec::new(),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn empty_param_list() {
        let content = "Complex[    ]";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Err(ParseError {
                message: "unexpected empty type parameter list".to_string(),
                kind: Unexpected,
                position: content
                    .find("[    ]")
                    .map(|i| i..i + "[    ]".len())
                    .unwrap()
            })
        );
    }

    #[test]
    fn parametrized_types() {
        let content = "MyType[A[X, Y[Any], foo::Z], B[std::C[D]]]";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Parametrized(ParametrizedType {
                path: vec![],
                name: "MyType",
                params: vec![
                    Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "A",
                        params: vec![
                            Type::Parametrized(ParametrizedType {
                                path: vec![],
                                name: "X",
                                params: Vec::new(),
                                segment: find_in(content, "X"),
                            }),
                            Type::Parametrized(ParametrizedType {
                                path: vec![],
                                name: "Y",
                                params: vec![Type::Parametrized(ParametrizedType {
                                    path: vec![],
                                    name: "Any",
                                    params: Vec::new(),
                                    segment: find_in(content, "Any"),
                                })],
                                segment: find_in(content, "Y[Any]"),
                            }),
                            Type::Parametrized(ParametrizedType {
                                path: vec![InclusionPathItem::Symbol(
                                    "foo",
                                    find_in(content, "foo")
                                ),],
                                name: "Z",
                                params: Vec::new(),
                                segment: find_in(content, "foo::Z"),
                            }),
                        ],
                        segment: find_in(content, "A[X, Y[Any], foo::Z]"),
                    }),
                    Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "B",
                        params: vec![Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol("std", find_in(content, "std")),],
                            name: "C",
                            params: vec![Type::Parametrized(ParametrizedType {
                                path: vec![],
                                name: "D",
                                params: Vec::new(),
                                segment: find_in(content, "D"),
                            })],
                            segment: find_in(content, "std::C[D]"),
                        })],
                        segment: find_in(content, "B[std::C[D]]"),
                    }),
                ],
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn type_params_missing_comma() {
        let content = "MyType[X Y]";
        let source = Source::unknown(content);
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
        let content = "Complex[  @  ]";
        let source = Source::unknown(content);
        let res = Parser::new(source).parse_specific(Parser::parse_type);
        assert_eq!(
            res,
            Err(ParseError {
                message: "'@' is not a valid type identifier.".to_string(),
                kind: Unexpected,
                position: content.find('@').map(|i| i..i + 1).unwrap(),
            })
        );
    }

    #[test]
    fn type_invalid_eod() {
        let content = "Complex[  x  }";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Err(ParseError {
                message: "Mismatched closing delimiter.".to_string(),
                kind: Unpaired(content.find('[').map(|i| i..i + 1).unwrap()),
                position: content.find('}').map(|i| i..i + 1).unwrap(),
            })
        );
    }

    #[test]
    fn simple_lambda_declaration() {
        let content = "A => B";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Callable(CallableType {
                params: vec![Type::Parametrized(ParametrizedType {
                    path: vec![],
                    name: "A",
                    params: Vec::new(),
                    segment: find_in(content, "A"),
                })],
                output: Box::new(Type::Parametrized(ParametrizedType {
                    path: vec![],
                    name: "B",
                    params: Vec::new(),
                    segment: find_in(content, "B"),
                })),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn by_name_declaration() {
        let content = " => B";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::ByName(ByName {
                name: Box::new(Type::Parametrized(ParametrizedType {
                    path: vec![],
                    name: "B",
                    params: Vec::new(),
                    segment: find_in(content, "B"),
                })),
                segment: 1..content.len(),
            }))
        );
    }

    #[test]
    fn nested_by_name_declaration() {
        let content = "=> => B";
        let source = Source::unknown(content);
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
        let content = "=> (=> B)";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::ByName(ByName {
                name: Box::new(Type::ByName(ByName {
                    name: Box::new(Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "B",
                        params: Vec::new(),
                        segment: find_in(content, "B"),
                    })),
                    segment: find_in(content, "=> B"),
                })),
                segment: /*source.segment()*/0..content.len() - 1,
            }))
        );
    }

    #[test]
    fn parentheses() {
        let content = "((((A))))";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Parametrized(ParametrizedType {
                path: vec![],
                name: "A",
                params: Vec::new(),
                segment: find_in(content, "A"),
            }))
        );
    }

    #[test]
    fn lambda_declaration_no_input() {
        let content = "() => B";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Callable(CallableType {
                params: vec![],
                output: Box::new(Type::Parametrized(ParametrizedType {
                    path: vec![],
                    name: "B",
                    params: Vec::new(),
                    segment: find_in(content, "B"),
                })),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn lambda_declaration_void_output() {
        let content = "A => Unit";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Callable(CallableType {
                params: vec![Type::Parametrized(ParametrizedType {
                    path: vec![],
                    name: "A",
                    params: Vec::new(),
                    segment: find_in(content, "A"),
                })],
                output: Box::new(Type::Parametrized(ParametrizedType {
                    path: vec![],
                    name: "Unit",
                    params: Vec::new(),
                    segment: find_in(content, "Unit"),
                })),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn lambda_multiple_inputs() {
        let content = "(A, B, C) => D";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_specific(Parser::parse_type),
            Ok(Type::Callable(CallableType {
                params: vec![
                    Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "A",
                        params: Vec::new(),
                        segment: find_in(content, "A"),
                    }),
                    Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "B",
                        params: Vec::new(),
                        segment: find_in(content, "B"),
                    }),
                    Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "C",
                        params: Vec::new(),
                        segment: find_in(content, "C"),
                    }),
                ],
                output: Box::new(Type::Parametrized(ParametrizedType {
                    path: vec![],
                    name: "D",
                    params: Vec::new(),
                    segment: find_in(content, "D"),
                })),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn chained_lambdas() {
        let content = "((A => B) => C) => D";
        let source = Source::unknown(content);
        let ast = Parser::new(source).parse_specific(Parser::parse_type);
        assert_eq!(
            ast,
            Ok(Type::Callable(CallableType {
                params: vec![Type::Callable(CallableType {
                    params: vec![Type::Callable(CallableType {
                        params: vec![Type::Parametrized(ParametrizedType {
                            path: vec![],
                            name: "A",
                            params: Vec::new(),
                            segment: find_in(content, "A"),
                        }),],
                        output: Box::new(Type::Parametrized(ParametrizedType {
                            path: vec![],
                            name: "B",
                            params: Vec::new(),
                            segment: find_in(content, "B"),
                        })),
                        segment: find_in(content, "A => B"),
                    })],
                    output: Box::new(Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "C",
                        params: Vec::new(),
                        segment: find_in(content, "C"),
                    })),
                    segment: find_in(content, "(A => B) => C"),
                })],
                output: Box::new(Type::Parametrized(ParametrizedType {
                    path: vec![],
                    name: "D",
                    params: Vec::new(),
                    segment: find_in(content, "D"),
                })),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn tuple_declaration() {
        let content = "(A, B, C)";
        let source = Source::unknown(content);
        let ast = Parser::new(source).parse_specific(Parser::parse_type);
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Tuples are not yet supported. A lambda declaration was expected here"
                    .to_string(),
                position: content.len()..content.len(),
                kind: Expected("(A, B, C) => <types>".to_string()),
            })
        );
    }

    #[test]
    fn parenthesised_lambda_input() {
        let content = "(A, B, C) => D => E";
        let source = Source::unknown(content);
        let ast = Parser::new(source).parse_specific(Parser::parse_type);
        assert_eq!(
            ast,
            Ok(Type::Callable(CallableType {
                params: vec![
                    Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "A",
                        params: Vec::new(),
                        segment: find_in(content, "A")
                    }),
                    Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "B",
                        params: Vec::new(),
                        segment: find_in(content, "B")
                    }),
                    Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "C",
                        params: Vec::new(),
                        segment: find_in(content, "C")
                    }),
                ],
                output: Box::new(Type::Callable(CallableType {
                    params: vec![Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "D",
                        params: Vec::new(),
                        segment: find_in(content, "D")
                    })],
                    output: Box::new(Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "E",
                        params: Vec::new(),
                        segment: find_in(content, "E")
                    })),
                    segment: find_in(content, "D => E")
                })),
                segment: source.segment(),
            }))
        );
    }

    #[test]
    fn unparenthesised_lambda_input() {
        let ast1 = Parser::new(Source::unknown("(A, B, C) => D => E => F")).parse_type();
        let ast2 = Parser::new(Source::unknown("A => B => C")).parse_type();
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
