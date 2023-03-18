use crate::err::ParseErrorKind;
use crate::moves::{of_type, MoveOperations, eod, word_seps, lookahead, repeat, of_types};
use crate::parser::{ParseResult, Parser};
use ast::r#type::Type;
use lexer::token::TokenType::{Comma, Identifier, RoundedLeftBracket, SquaredLeftBracket, SquaredRightBracket};
use crate::err::ParseErrorKind::Excepted;

pub trait TypeAspect<'a> {
    fn parse_type(&mut self) -> ParseResult<Type<'a>>;
    fn parse_type_parameter_list(&mut self) -> ParseResult<Vec<Type<'a>>>;
}

impl<'a> TypeAspect<'a> for Parser<'a> {
    fn parse_type(&mut self) -> ParseResult<Type<'a>> {
        self.cursor.advance(word_seps()); //consume word seps


        //a variable that determines if the parsed type is a polytype
        let mut is_polytype = false;

        let name_token = self.cursor.next()?;
        if name_token.token_type != Identifier {
            return Err(self.mk_parse_error(format!(
                "'{}' is not a valid type identifier.",
                name_token.value
            ), name_token, ParseErrorKind::Unexpected));
        }

        Ok(Type {
            name: name_token.value,
            params: self.parse_type_parameter_list()?,
        })
    }

    fn parse_type_parameter_list(&mut self) -> ParseResult<Vec<Type<'a>>> {
        let start = match self.cursor.advance(of_type(SquaredLeftBracket)) {
            Some(start) => start,
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
                Excepted("',' or ']'")
            )?;
        }
        self.cursor.advance(word_seps());

        if tparams.is_empty() {
            self.expect_delimiter(SquaredRightBracket)?;
            return self.expected_with(
                "unexpected empty type parameter list",
                start..self.cursor.peek(),
                ParseErrorKind::Unexpected,
            );
        }

        self.cursor.advance(word_seps());
        self.expect_delimiter(SquaredRightBracket)?;

        Ok(tparams)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use ast::r#type::Type;
    use context::source::Source;
    use crate::aspects::r#type::TypeAspect;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::err::ParseErrorKind::Excepted;
    use crate::parser::{Parser};

    #[test]
    fn simple_type() {
        let content = "MyType";
        let source = Source::unknown(content);
        assert_eq!(
            Parser::new(source).parse_type(),
            Ok(Type {
                name: "MyType",
                params: Vec::new(),
            })
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
            Ok(Type {
                name: "MyType",
                params: vec![
                    Type {
                        name: "A",
                        params: vec![
                            Type {
                                name: "X",
                                params: Vec::new(),
                            },
                            Type {
                                name: "Y",
                                params: vec![Type {
                                    name: "_",
                                    params: Vec::new(),
                                }],
                            },
                            Type {
                                name: "Z",
                                params: Vec::new(),
                            },
                        ],
                    },
                    Type {
                        name: "B",
                        params: vec![Type {
                            name: "C",
                            params: vec![Type {
                                name: "D",
                                params: Vec::new(),
                            }],
                        }],
                    },
                ],
            })
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
                message: "Unexpected closing delimiter.".to_string(),
                kind: Excepted("]"),
                position: content.find('}').map(|i| i..i + 1).unwrap()
            })
        );
    }

}
