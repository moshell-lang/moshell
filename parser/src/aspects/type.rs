use crate::err::ParseErrorKind::{Expected, Unexpected};
use crate::moves::{any, eod, lookahead, not, of_type, word_seps, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::r#type::{ByName, CallableType, SimpleType, Type};
use lexer::token::TokenType;
use lexer::token::TokenType::{
    Comma, FatArrow, Identifier, NewLine, RoundedLeftBracket, RoundedRightBracket,
    SquaredLeftBracket, SquaredRightBracket, Unit, Nothing,
};
use std::fmt::Write;

///A parser aspect to parse all types declarations, such as lambdas, constant types, parametrized types and Unit
pub trait TypeAspect<'a> {
    ///parse a lambda types signature, a constant or parametrized types or Unit.
    fn parse_type(&mut self) -> ParseResult<Type<'a>>;

    ///parse a types parameter list (`[...]`)
    fn parse_type_parameter_list(&mut self) -> ParseResult<Vec<Type<'a>>>;
}

impl<'a> TypeAspect<'a> for Parser<'a> {
    fn parse_type(&mut self) -> ParseResult<Type<'a>> {
        self.cursor.advance(word_seps()); //consume word seps

        let first_token = self.cursor.peek();
        //if there's a parenthesis then the types is necessarily a lambda types
        let mut tpe = match first_token.token_type {
            RoundedLeftBracket => self.parse_parentheses()?,
            FatArrow => self.parse_by_name().map(Type::ByName)?,
            _ => self.parse_simple_or_unit()?,
        };
        //check if there's an arrow, if some, we are maybe in a case where the types is "A => ..."
        // (a lambda with one parameter and no parenthesis for the input, which is valid)
        if self
            .cursor
            .advance(word_seps().then(of_type(FatArrow)))
            .is_none()
        {
            return Ok(tpe);
        }

        if matches!(tpe, Type::Simple(..)) {
            tpe = Type::Callable(CallableType {
                params: vec![tpe],
                output: Box::new(self.parse_simple_or_unit()?),
            })
        }

        //check if there's an arrow, if some, we are maybe in a case where the types is "A => ..."
        // (a lambda with one parameter and no parenthesis for the input, which is valid)
        if self
            .cursor
            .advance(word_seps().then(of_type(FatArrow)))
            .is_some()
        {
            //parse second lambda output in order to advance on the full invalid lambda expression
            let second_rt = self.parse_simple_or_unit()?;
            return self.expected_with(
                "Lambda types as input of another lambda must be surrounded with parenthesis",
                first_token..self.cursor.peek(),
                Expected(self.unparenthesised_lambda_input_tip(tpe, second_rt)),
            );
        }

        Ok(tpe)
    }

    fn parse_type_parameter_list(&mut self) -> ParseResult<Vec<Type<'a>>> {
        self.parse_type_list(SquaredLeftBracket, SquaredRightBracket, true)
    }
}

impl<'a> Parser<'a> {
    fn parse_by_name(&mut self) -> ParseResult<ByName<'a>> {
        self.cursor
            .force(of_type(FatArrow), "'=>' expected here.")?;

        //control case of => => A which is invalid
        self.cursor
            .force(word_seps().then(not(of_type(FatArrow))), "unexpected '=>'")?;

        Ok(ByName {
            name: self.parse_type().map(Box::new)?,
        })
    }

    fn unparenthesised_lambda_input_tip(
        &self,
        left_lambda: Type<'a>,
        lambda_out: Type<'a>,
    ) -> String {
        "(".to_string() + &left_lambda.to_string() + ") => " + &lambda_out.to_string()
    }

    fn parse_parentheses(&mut self) -> ParseResult<Type<'a>> {
        let inputs = self.parse_type_list(RoundedLeftBracket, RoundedRightBracket, false)?;

        //if there is an arrow then it is a lambda
        if self
            .cursor
            .advance(word_seps().then(of_type(FatArrow)))
            .is_some()
        {
            return self.parse_lambda_with_inputs(inputs).map(Type::Callable);
        }

        //there is no inputs (`()`) and no `=>` after, this is a Unit types
        if inputs.is_empty() {
            return Ok(Type::Unit);
        }

        //its a types of form `(A)`
        if let Some(ty) = inputs.first() {
            if inputs.len() == 1 {
                return Ok(ty.clone());
            }
        }

        let mut rendered_tuple = String::new();
        rendered_tuple += "(";

        if let Some((first, rest)) = inputs.split_first() {
            write!(rendered_tuple, "{}", first).unwrap();
            for tpe in rest {
                write!(rendered_tuple, ", {}", tpe).unwrap();
            }
        }

        rendered_tuple += ") => <types>";
        self.expected(
            "Tuples are not yet supported. A lambda declaration was expected here",
            Expected(rendered_tuple),
        )
    }

    fn parse_lambda_with_inputs(&mut self, inputs: Vec<Type<'a>>) -> ParseResult<CallableType<'a>> {
        Ok(CallableType {
            params: inputs,
            output: Box::new(self.parse_type()?),
        })
    }

    fn parse_simple_or_unit(&mut self) -> ParseResult<Type<'a>> {
        let name_token = self.cursor.advance(word_seps().then(any())).unwrap();

        return match name_token.token_type {
            Identifier => Ok(Type::Simple(SimpleType {
                name: name_token.value,
                params: self.parse_type_parameter_list()?,
            })),
            Unit => Ok(Type::Unit),
            Nothing => Ok(Type::Nothing),

            NewLine => self.expected_with("expected types", name_token, Expected("<types>".to_string())),

            x if x.is_closing_ponctuation() => self.expected_with("expected types", name_token, Expected("<types>".to_string())),
            x if (x == RoundedLeftBracket
                && self
                .cursor
                .advance(word_seps().then(of_type(RoundedRightBracket)))
                .is_some())
            => Ok(Type::Unit),

            _ => self.expected_with(
                &format!("'{}' is not a valid types identifier.", &name_token.value),
                name_token.value,
                Unexpected,
            )
        };
    }

    fn parse_type_list(
        &mut self,
        start: TokenType,
        end: TokenType,
        non_empty: bool,
    ) -> ParseResult<Vec<Type<'a>>> {
        let start = match self.cursor.advance(of_type(start)) {
            Some(start) => {
                self.delimiter_stack.push_back(start.clone());
                start
            }
            None => return Ok(Vec::new()),
        };

        let mut tparams = vec![];

        while self.cursor.lookahead(word_seps().then(eod())).is_none() {
            tparams.push(self.parse_type()?);
            self.cursor.force_with(
                word_seps().then(of_type(Comma).or(lookahead(eod()))),
                "A comma or a closing bracket was expected here",
                Expected("',' or ']'".to_string()),
            )?;
        }
        self.cursor.advance(word_seps());

        if tparams.is_empty() && non_empty {
            self.expect_delimiter(end)?;
            return self.expected_with(
                "unexpected empty types parameter list",
                start..self.cursor.peek(),
                Unexpected,
            );
        }

        self.cursor.advance(word_seps());
        self.expect_delimiter(end)?;

        Ok(tparams)
    }
}

#[cfg(test)]
mod tests {
    use crate::aspects::r#type::TypeAspect;
    use crate::err::ParseErrorKind::{Expected, Unexpected, Unpaired};
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parser::Parser;
    use ast::r#type::{ByName, CallableType, SimpleType, Type};
    use context::source::Source;
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_type() {
        let content = "MyType";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
            Ok(Type::Simple(SimpleType {
                name: "MyType",
                params: Vec::new(),
            }))
        );
    }

    #[test]
    fn empty_param_list() {
        let content = "Complex[    ]";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
            Err(ParseError {
                message: "unexpected empty types parameter list".to_string(),
                kind: ParseErrorKind::Unexpected,
                position: content
                    .find("[    ]")
                    .map(|i| i..i + "[    ]".len())
                    .unwrap()
            })
        );
    }

    #[test]
    fn parametrized_types() {
        let content = "MyType[A[X, Y[_], Z], B[C[D]]]";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
            Ok(Type::Simple(SimpleType {
                name: "MyType",
                params: vec![
                    Type::Simple(SimpleType {
                        name: "A",
                        params: vec![
                            Type::Simple(SimpleType {
                                name: "X",
                                params: Vec::new(),
                            }),
                            Type::Simple(SimpleType {
                                name: "Y",
                                params: vec![Type::Simple(SimpleType {
                                    name: "_",
                                    params: Vec::new(),
                                })],
                            }),
                            Type::Simple(SimpleType {
                                name: "Z",
                                params: Vec::new(),
                            }),
                        ],
                    }),
                    Type::Simple(SimpleType {
                        name: "B",
                        params: vec![Type::Simple(SimpleType {
                            name: "C",
                            params: vec![Type::Simple(SimpleType {
                                name: "D",
                                params: Vec::new(),
                            })],
                        })],
                    }),
                ],
            }))
        );
    }

    #[test]
    fn type_params_missing_comma() {
        let content = "MyType[X Y]";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
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
        let res = Parser::new(source).parse_type();
        assert_eq!(
            res,
            Err(ParseError {
                message: "'@' is not a valid types identifier.".to_string(),
                kind: ParseErrorKind::Unexpected,
                position: content.find('@').map(|i| i..i + 1).unwrap(),
            })
        );
    }

    #[test]
    fn type_invalid_eod() {
        let content = "Complex[  x  }";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
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
            Parser::new(source).parse_type(),
            Ok(Type::Callable(CallableType {
                params: vec![Type::Simple(SimpleType {
                    name: "A",
                    params: Vec::new(),
                })],
                output: Box::new(Type::Simple(SimpleType {
                    name: "B",
                    params: Vec::new(),
                })),
            }))
        );
    }

    #[test]
    fn by_name_declaration() {
        let content = " => B";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
            Ok(Type::ByName(ByName {
                name: Box::new(Type::Simple(SimpleType {
                    name: "B",
                    params: Vec::new(),
                })),
            }))
        );
    }

    #[test]
    fn nested_by_name_declaration() {
        let content = "=> => B";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
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
            Parser::new(source).parse_type(),
            Ok(Type::ByName(ByName {
                name: Box::new(Type::ByName(ByName {
                    name: Box::new(Type::Simple(SimpleType {
                        name: "B",
                        params: Vec::new(),
                    }))
                }))
            }))
        );
    }

    #[test]
    fn parentheses() {
        let content = "((((A))))";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
            Ok(Type::Simple(SimpleType {
                name: "A",
                params: Vec::new(),
            }))
        );
    }

    #[test]
    fn lambda_declaration_no_input() {
        let content = "() => B";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
            Ok(Type::Callable(CallableType {
                params: vec![],
                output: Box::new(Type::Simple(SimpleType {
                    name: "B",
                    params: Vec::new(),
                })),
            }))
        );
    }

    #[test]
    fn unit_type() {
        let res1 = Parser::new(Source::unknown("()")).parse_type();
        let res2 = Parser::new(Source::unknown("Unit")).parse_type();
        let res3 = Parser::new(Source::unknown("(\\\n   \\\n)")).parse_type();
        assert_eq!(res1, Ok(Type::Unit));
        assert_eq!(res2, Ok(Type::Unit));
        assert_eq!(res3, Ok(Type::Unit));
    }

    #[test]
    fn lambda_declaration_void_output() {
        let content = "A => ()";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
            Ok(Type::Callable(CallableType {
                params: vec![Type::Simple(SimpleType {
                    name: "A",
                    params: Vec::new(),
                })],
                output: Box::new(Type::Unit),
            }))
        );
    }

    #[test]
    fn lambda_multiple_inputs() {
        let content = "(A, B, C) => D";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
            Ok(Type::Callable(CallableType {
                params: vec![
                    Type::Simple(SimpleType {
                        name: "A",
                        params: Vec::new(),
                    }),
                    Type::Simple(SimpleType {
                        name: "B",
                        params: Vec::new(),
                    }),
                    Type::Simple(SimpleType {
                        name: "C",
                        params: Vec::new(),
                    }),
                ],
                output: Box::new(Type::Simple(SimpleType {
                    name: "D",
                    params: Vec::new(),
                })),
            }))
        );
    }

    #[test]
    fn chained_lambdas() {
        let content = "((A => B) => C) => D";
        let source = Source::unknown(content);
        let ast = Parser::new(source).parse_type();
        assert_eq!(
            ast,
            Ok(Type::Callable(CallableType {
                params: vec![Type::Callable(CallableType {
                    params: vec![Type::Callable(CallableType {
                        params: vec![Type::Simple(SimpleType {
                            name: "A",
                            params: Vec::new(),
                        }),],
                        output: Box::new(Type::Simple(SimpleType {
                            name: "B",
                            params: Vec::new(),
                        })),
                    })],
                    output: Box::new(Type::Simple(SimpleType {
                        name: "C",
                        params: Vec::new(),
                    })),
                })],
                output: Box::new(Type::Simple(SimpleType {
                    name: "D",
                    params: Vec::new(),
                })),
            }))
        );
    }

    #[test]
    fn tuple_declaration() {
        let content = "(A, B, C)";
        let source = Source::unknown(content);
        let ast = Parser::new(source).parse_type();
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
        let ast = Parser::new(source).parse_type();
        assert_eq!(
            ast,
            Ok(Type::Callable(CallableType {
                params: vec![
                    Type::Simple(SimpleType {
                        name: "A",
                        params: Vec::new(),
                    }),
                    Type::Simple(SimpleType {
                        name: "B",
                        params: Vec::new(),
                    }),
                    Type::Simple(SimpleType {
                        name: "C",
                        params: Vec::new(),
                    }),
                ],
                output: Box::new(Type::Callable(CallableType {
                    params: vec![Type::Simple(SimpleType {
                        name: "D",
                        params: Vec::new()
                    })],
                    output: Box::new(Type::Simple(SimpleType {
                        name: "E",
                        params: Vec::new()
                    })),
                })),
            }))
        );
    }

    #[test]
    fn unparenthesised_lambda_input() {
        let ast1 = Parser::new(Source::unknown("(A, B, C) => D => E => F")).parse_type();
        let ast2 = Parser::new(Source::unknown("A => B => C")).parse_type();
        let expected1 = Err(ParseError {
            message: "Lambda types as input of another lambda must be surrounded with parenthesis"
                .to_string(),
            kind: Expected("(D => E) => F".to_string()),
            position: 13..24,
        });
        let expected2 = Err(ParseError {
            message: "Lambda types as input of another lambda must be surrounded with parenthesis"
                .to_string(),
            kind: Expected("(A => B) => C".to_string()),
            position: 0..11,
        });
        assert_eq!(ast1, expected1);
        assert_eq!(ast2, expected2);
    }
}
