use crate::err::ParseErrorKind;
use crate::moves::{of_type, MoveOperations, eod, word_seps, lookahead};
use crate::parser::{ParseResult, Parser};
use ast::r#type::{Monotype, Polytype, Type};
use lexer::token::TokenType;
use lexer::token::TokenType::{Comma, FatArrow, Identifier, RoundedLeftBracket,
                              RoundedRightBracket, SquaredLeftBracket, SquaredRightBracket};
use crate::err::ParseErrorKind::Excepted;

pub trait TypeAspect<'a> {
    fn parse_type(&mut self) -> ParseResult<Type<'a>>;
    fn parse_type_parameter_list(&mut self) -> ParseResult<Vec<Type<'a>>>;
}

impl<'a> TypeAspect<'a> for Parser<'a> {
    fn parse_type(&mut self) -> ParseResult<Type<'a>> {
        self.cursor.advance(word_seps()); //consume word seps


        let parenthesis = self.cursor.lookahead(of_type(RoundedLeftBracket)).is_some();
        //if there's a parenthesis then the type is necessarily a polytype
        if parenthesis {
            return self.parse_polytype().map(Type::Polytype)
        }

        let tpe = Type::Monotype(self.parse_monotype()?);

        //check if there's an arrow, if some, we are maybe in a case where the type is "A => ..."
        // (a lambda with one parameter and no parenthesis for the input, which is valid)
        if self.cursor.lookahead(word_seps().then(of_type(FatArrow))).is_none() {
            return Ok(tpe)
        }

        let first = self.parse_polytype_with_inputs(vec![tpe])
            .map(Type::Polytype)?;

        //lookahead for another lambda, if some,
        //we are in a case where there is a lambda as input of another, such as "A => B => ...",
        //let's pass the first lambda as input of the next one
        if self.cursor.lookahead(word_seps().then(of_type(FatArrow))).is_some() {
            return self.parse_polytype_with_inputs(vec![first]).map(Type::Polytype)
        }

        Ok(first)
    }

    fn parse_type_parameter_list(&mut self) -> ParseResult<Vec<Type<'a>>> {
        self.parse_type_list(SquaredLeftBracket, SquaredRightBracket)
    }
}

impl<'a> Parser<'a> {
    fn parse_polytype(&mut self) -> ParseResult<Polytype<'a>> {
        let inputs = self.parse_type_list(RoundedLeftBracket, RoundedRightBracket)?;
        self.parse_polytype_with_inputs(inputs)
    }

    fn parse_polytype_with_inputs(&mut self, inputs: Vec<Type<'a>>) -> ParseResult<Polytype<'a>> {
        self.cursor.force_with(
            word_seps().then(of_type(FatArrow)),
            "Tuples are not yet supported. A lambda declaration was expected here",
            Excepted("=> <output_type>"),
        )?;
        Ok(Polytype {
            inputs,
            output: Box::new(self.parse_type()?),
        })
    }

    fn parse_monotype(&mut self) -> ParseResult<Monotype<'a>> {
        let name_token = self.cursor.next()?;
        if name_token.token_type != Identifier {
            return Err(self.mk_parse_error(format!(
                "'{}' is not a valid type identifier.",
                name_token.value
            ), name_token, ParseErrorKind::Unexpected));
        }

        Ok(Monotype {
            name: name_token.value,
            params: self.parse_type_parameter_list()?,
        })
    }

    fn parse_type_list(&mut self, start: TokenType, end: TokenType) -> ParseResult<Vec<Type<'a>>> {
        let start = match self.cursor.advance(of_type(start)) {
            Some(start) => {
                self.delimiter_stack.push_back(start.clone());
                start
            }
            None => return Ok(Vec::new()),
        };

        let mut tparams = vec![];

        while self
            .cursor
            .lookahead(word_seps().then(eod()))
            .is_none()
        {
            tparams.push(self.parse_type()?);
            self.cursor.force_with(
                word_seps().then(of_type(Comma).or(lookahead(eod()))),
                "A comma or a closing bracket was expected here",
                Excepted("',' or ']'"),
            )?;
        }
        self.cursor.advance(word_seps());

        if tparams.is_empty() {
            self.expect_delimiter(end)?;
            return self.expected_with(
                "unexpected empty type parameter list",
                start..self.cursor.peek(),
                ParseErrorKind::Unexpected,
            );
        }

        self.cursor.advance(word_seps());
        self.expect_delimiter(end)?;

        Ok(tparams)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use ast::r#type::{Monotype, Polytype, Type};
    use context::source::Source;
    use crate::aspects::r#type::TypeAspect;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::err::ParseErrorKind::{Excepted, Unpaired};
    use crate::parser::{Parser};

    #[test]
    fn simple_type() {
        let content = "MyType";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
            Ok(Type::Monotype(Monotype {
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
                message: "unexpected empty type parameter list".to_string(),
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
            Ok(Type::Monotype(Monotype {
                name: "MyType",
                params: vec![
                    Type::Monotype(Monotype {
                        name: "A",
                        params: vec![
                            Type::Monotype(Monotype {
                                name: "X",
                                params: Vec::new(),
                            }),
                            Type::Monotype(Monotype {
                                name: "Y",
                                params: vec![Type::Monotype(Monotype {
                                    name: "_",
                                    params: Vec::new(),
                                })],
                            }),
                            Type::Monotype(Monotype {
                                name: "Z",
                                params: Vec::new(),
                            }),
                        ],
                    }),
                    Type::Monotype(Monotype {
                        name: "B",
                        params: vec![Type::Monotype(Monotype {
                            name: "C",
                            params: vec![Type::Monotype(Monotype {
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
                kind: Excepted("',' or ']'"),
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
                message: "'@' is not a valid type identifier.".to_string(),
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
            Ok(Type::Polytype(Polytype {
                inputs: vec![Type::Monotype(Monotype {
                    name: "A",
                    params: Vec::new(),
                })],
                output: Box::new(Type::Monotype(Monotype {
                    name: "B",
                    params: Vec::new(),
                })),
            }))
        );
    }

    #[test]
    fn lambda_multiple_inputs() {
        let content = "(A, B, C) => D";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
            Ok(Type::Polytype(Polytype {
                inputs: vec![
                    Type::Monotype(Monotype {
                        name: "A",
                        params: Vec::new(),
                    }),
                    Type::Monotype(Monotype {
                        name: "B",
                        params: Vec::new(),
                    }),
                    Type::Monotype(Monotype {
                        name: "C",
                        params: Vec::new(),
                    }),
                ],
                output: Box::new(Type::Monotype(Monotype {
                    name: "D",
                    params: Vec::new(),
                })),
            }))
        );
    }

    #[test]
    fn chained_lambdas() {
        let content = "A => B => C => D";
        let source = Source::unknown(content);
        let ast = Parser::new(source).parse_type();
        assert_eq!(
            ast,
            Ok(Type::Polytype(Polytype {
                inputs: vec![
                    Type::Polytype(Polytype {
                        inputs: vec![
                            Type::Polytype(Polytype {
                                inputs: vec![
                                    Type::Monotype(Monotype {
                                        name: "A",
                                        params: Vec::new(),
                                    }),
                                ],
                                output: Box::new(Type::Monotype(Monotype {
                                    name: "B",
                                    params: Vec::new(),
                                })),
                            })
                        ],
                        output: Box::new(Type::Monotype(Monotype {
                            name: "C",
                            params: Vec::new(),
                        })),
                    })
                ],
                output: Box::new(Type::Monotype(Monotype {
                    name: "D",
                    params: Vec::new(),
                })),
            }))
        );
    }
}
