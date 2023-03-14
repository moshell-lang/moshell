use crate::err::ParseErrorKind;
use crate::moves::{of_type, MoveOperations, eod, word_seps};
use crate::parser::{ParseResult, Parser};
use ast::r#type::Type;
use lexer::token::TokenType::{Comma, Identifier, SquaredLeftBracket, SquaredRightBracket};

pub trait TypeAspect<'a> {
    fn parse_type(&mut self) -> ParseResult<Type<'a>>;
    fn parse_type_parameter_list(&mut self) -> ParseResult<Vec<Type<'a>>>;
}

impl<'a> TypeAspect<'a> for Parser<'a> {
    fn parse_type(&mut self) -> ParseResult<Type<'a>> {
        self.cursor.advance(word_seps()); //consume word seps

        let name = self
            .cursor
            .force(
                of_type(Identifier),
                &format!(
                    "'{}' is not a valid type identifier.",
                    self.cursor.peek().value
                ),
            )?
            .value;

        Ok(Type {
            name,
            params: self.parse_type_parameter_list()?,
        })
    }

    fn parse_type_parameter_list(&mut self) -> ParseResult<Vec<Type<'a>>> {
        let start = match self.cursor.advance(of_type(SquaredLeftBracket)) {
            Some(start) => start,
            None => return Ok(Vec::new()),
        };

        self.cursor.advance(word_seps());
        if self
            .cursor
            .lookahead(eod())
            .is_some()
        {
            self.expect_delimiter(SquaredRightBracket)?;
            return self.expected_with(
                "unexpected empty type parameter list",
                start..self.cursor.peek(),
                ParseErrorKind::Unexpected,
            );
        }
        let mut tparams = vec![self.parse_type()?];

        while self
            .cursor
            .lookahead(eod())
            .is_none()
        {
            self.cursor.force(
                word_seps().then(of_type(Comma)),
                "A comma or a closing bracket was expected here",
            )?;
            tparams.push(self.parse_type()?);
        }

        self.expect_delimiter(SquaredRightBracket)?;

        Ok(tparams)
    }
}

#[cfg(test)]
mod tests {
    use ast::r#type::Type;
    use context::source::Source;
    use crate::aspects::r#type::TypeAspect;
    use crate::parser::Parser;

    #[test]
    fn test_simple_type() {
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
    fn test_parametrized_types() {
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
}
