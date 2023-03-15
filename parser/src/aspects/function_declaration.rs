use ast::function::{FunctionDeclaration, FunctionParameter};
use ast::r#type::Type;
use lexer::token::TokenType;
use lexer::token::TokenType::{Arrow, Comma, Equal, Fun, Identifier, NewLine, RoundedLeftBracket, RoundedRightBracket, Space, SquaredLeftBracket, SquaredRightBracket, Vararg};

use crate::aspects::r#type::TypeAspect;
use crate::aspects::var_declaration::VarDeclarationAspect;
use crate::err::ParseErrorKind;
use crate::moves::{blank, blanks, eod, like, lookahead, MoveOperations, not, of_type, of_types, repeat};
use crate::parser::{Parser, ParseResult};

pub trait FunctionDeclarationAspect<'a> {
    fn parse_function_declaration(&mut self) -> ParseResult<FunctionDeclaration<'a>>;
}

impl<'a> FunctionDeclarationAspect<'a> for Parser<'a> {
    fn parse_function_declaration(&mut self) -> ParseResult<FunctionDeclaration<'a>> {
        self.cursor.force(of_type(Fun), "expected 'fun' keyword at start of function declaration.")?;

        //consume blanks
        self.cursor.advance(blanks());


        let name = self.parse_fn_declaration_name()?;
        let tparams = self.parse_type_parameter_list()?;
        let params = self.parse_fn_parameter_list()?;
        let rtype = self.parse_fn_return_type()?;
        let body = self.cursor
            .force(blanks().then(of_type(Equal)), "expected '='")
            .and_then(|_| self.statement())
            .map(Box::new)?;

        Ok(FunctionDeclaration {
            name,
            type_parameters: tparams,
            parameters: params,
            return_type: rtype,
            body,
        })
    }
}

impl<'a> Parser<'a> {
    fn parse_fn_return_type(&mut self) -> ParseResult<Option<Type<'a>>> {
        if self.cursor.advance(blanks().then(of_type(Arrow))).is_none() {
            return Ok(None)
        }
        self.cursor.advance(blanks()); // consume blanks
        self.parse_type().map(Some)
    }

    fn parse_fn_parameter(&mut self) -> ParseResult<FunctionParameter<'a>> {
        self.cursor.advance(blanks()); //consume blanks

        let is_vararg = self.cursor.lookahead(
            repeat(//                           skip everything that could compose a type expression
                   of_types(&[Space, NewLine, Identifier, SquaredLeftBracket, SquaredRightBracket])
            ).then(of_type(Vararg))
        ).is_some();


        if is_vararg {
            let param = self.parse_type()
                .map(Some)
                .map(FunctionParameter::Variadic)?;
            self.cursor.force(of_type(Vararg), "expected '...'")?;
            return Ok(param)
        }

        self.parse_typed_var()
            .map(FunctionParameter::Named)
    }

    fn parse_fn_parameter_list(&mut self) -> ParseResult<Vec<FunctionParameter<'a>>> {
        self.cursor.force(of_type(RoundedLeftBracket), "expected start of parameter list ('(')")?;

        let mut params = Vec::new();
        while self.cursor.lookahead(blanks().then(eod())).is_none() {
            let param = self.parse_fn_parameter()?;
            params.push(param);
            self.cursor.force(
                blanks().then(of_type(Comma).or(lookahead(eod()))),
                "expected ','",
            )?;
        }

        self.cursor.advance(blanks()); //consume blanks
        self.expect_delimiter(RoundedRightBracket)?;

        Ok(params)
    }

    fn parse_fn_declaration_name(&mut self) -> ParseResult<&'a str> {
        self.cursor
            .advance(like(TokenType::is_valid_function_name))
            .ok_or_else(|| {
                let wrong_name_slice = self.cursor.select(not(blank().or(like(TokenType::is_ponctuation)))).to_owned();
                self.mk_parse_error(
                    "function name is invalid.",
                    wrong_name_slice.as_slice(),
                    ParseErrorKind::Unexpected,
                )
            }).map(|t| t.value)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::Call;
    use ast::Expr;
    use ast::function::{FunctionDeclaration, FunctionParameter};
    use ast::r#type::Type;
    use ast::variable::{TypedVariable, VarReference};
    use context::source::Source;

    use crate::parse;

    #[test]
    fn function_declaration() {
        let source = Source::unknown("\
        fun test() = x
        ");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: "test",
                type_parameters: vec![],
                parameters: vec![],
                return_type: None,
                body: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("x".into())],
                    type_parameters: vec![],
                })),
            })]
        )
    }

    #[test]
    fn function_declaration_param() {
        let source = Source::unknown("\
        fun test(x) = $x
        ");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: "test",
                type_parameters: vec![],
                parameters: vec![FunctionParameter::Named(TypedVariable {
                    name: "x",
                    ty: None,
                })],
                return_type: None,
                body: Box::new(Expr::VarReference(VarReference {
                    name: "x"
                })),
            })]
        )
    }

    #[test]
    fn function_declaration_params() {
        let source = Source::unknown("\
        fun test(  x : String  ,  y : Test   ) = x
        ");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: "test",
                type_parameters: vec![],
                parameters: vec![
                    FunctionParameter::Named(TypedVariable {
                        name: "x",
                        ty: Some(Type {
                            name: "String",
                            params: vec![],
                        }),
                    }),
                    FunctionParameter::Named(TypedVariable {
                        name: "y",
                        ty: Some(Type {
                            name: "Test",
                            params: vec![],
                        }),
                    }),
                ],
                return_type: None,
                body: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("x".into())],
                    type_parameters: vec![],
                })),
            })]
        )
    }

    #[test]
    fn function_declaration_tparams() {
        let source = Source::unknown("\
        fun test[X, Y](  x : X  ,  y : Y   ) = x
        ");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: "test",
                type_parameters: vec![
                    Type {
                        name: "X",
                        params: Vec::new(),
                    },
                    Type {
                        name: "Y",
                        params: Vec::new(),
                    },
                ],
                parameters: vec![
                    FunctionParameter::Named(TypedVariable {
                        name: "x",
                        ty: Some(Type {
                            name: "X",
                            params: vec![],
                        }),
                    }),
                    FunctionParameter::Named(TypedVariable {
                        name: "y",
                        ty: Some(Type {
                            name: "Y",
                            params: vec![],
                        }),
                    }),
                ],
                return_type: None,
                body: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("x".into())],
                    type_parameters: vec![],
                })),
            })]
        )
    }

    #[test]
    fn function_declaration_vararg() {
        let source = Source::unknown("\
        fun test(X...) = $x
        ");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: "test",
                type_parameters: vec![],
                parameters: vec![FunctionParameter::Variadic(Some(Type {
                    name: "X",
                    params: Vec::new(),
                }))],
                return_type: None,
                body: Box::new(Expr::VarReference(VarReference {
                    name: "x"
                })),
            })]
        )
    }

    #[test]
    fn function_declaration_complete() {
        let source = Source::unknown("\
        fun test[X, Y](  x : X  ,  y : Y   ) -> X = x
        ");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: "test",
                type_parameters: vec![
                    Type {
                        name: "X",
                        params: Vec::new(),
                    },
                    Type {
                        name: "Y",
                        params: Vec::new(),
                    },
                ],
                parameters: vec![
                    FunctionParameter::Named(TypedVariable {
                        name: "x",
                        ty: Some(Type {
                            name: "X",
                            params: vec![],
                        }),
                    }),
                    FunctionParameter::Named(TypedVariable {
                        name: "y",
                        ty: Some(Type {
                            name: "Y",
                            params: vec![],
                        }),
                    }),
                ],
                return_type: Some(Type {
                    name: "X",
                    params: Vec::new(),
                }),
                body: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("x".into())],
                    type_parameters: vec![],
                })),
            })]
        )
    }
}