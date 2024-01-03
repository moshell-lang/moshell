use ast::function::{FunctionDeclaration, FunctionParameter, Return};
use ast::r#type::Type;
use ast::variable::Identifier;
use context::source::SourceSegmentHolder;
use lexer::token::{Token, TokenType};

use crate::aspects::expr_list::ExpressionListAspect;
use crate::aspects::r#type::TypeAspect;
use crate::aspects::var_declaration::VarDeclarationAspect;
use crate::err::ParseErrorKind;
use crate::moves::{
    blank, blanks, eox, like, next, not, of_type, of_types, repeat, spaces, MoveOperations,
};
use crate::parser::{ParseResult, Parser};

///A parser aspect for function declarations
pub trait FunctionDeclarationAspect<'a> {
    ///Parse a function declaration
    fn parse_function_declaration(&mut self) -> ParseResult<FunctionDeclaration<'a>>;

    ///Parse a return expression
    fn parse_return(&mut self) -> ParseResult<Return<'a>>;
}

impl<'a> FunctionDeclarationAspect<'a> for Parser<'a> {
    fn parse_function_declaration(&mut self) -> ParseResult<FunctionDeclaration<'a>> {
        let fun = self.cursor.force(
            of_type(TokenType::Fun),
            "expected 'fun' keyword at start of function declaration.",
        )?;

        let segment_start = fun.span.start;

        //consume blanks for each function declaration components
        self.cursor.advance(blanks());
        let name = self.parse_fn_declaration_name()?;
        self.cursor.advance(blanks());

        let (tparams, _) = self.parse_optional_list(
            TokenType::SquaredLeftBracket,
            TokenType::SquaredRightBracket,
            "Expected type parameter.",
            Parser::parse_type_parameter,
        )?;
        self.cursor.advance(blanks());
        let params = self.parse_fn_parameter_list()?;
        self.cursor.advance(blanks());
        let rtype = self.parse_fn_return_type()?;

        if let Some(token) = self
            .cursor
            .lookahead(blanks().then(of_type(TokenType::SemiColon)))
        {
            return Ok(FunctionDeclaration {
                name: Identifier::extract(self.source.source, name.span),
                type_parameters: tparams,
                parameters: params,
                return_type: rtype,
                body: None,
                segment: segment_start..token.span.end,
            });
        }

        let body = self
            .cursor
            .force(blanks().then(of_type(TokenType::Equal)), "expected '='")
            .and_then(|_| {
                self.cursor.advance(blanks());
                self.statement()
            })
            .map(Box::new)?;

        let segment = segment_start..body.segment().end;

        Ok(FunctionDeclaration {
            name: Identifier::extract(self.source.source, name.span),
            type_parameters: tparams,
            parameters: params,
            return_type: rtype,
            body: Some(body),
            segment,
        })
    }

    fn parse_return(&mut self) -> ParseResult<Return<'a>> {
        let start = self
            .cursor
            .force(of_type(TokenType::Return), "'return' keyword expected here")?;
        if self.cursor.advance(spaces()).is_none() || self.cursor.lookahead(eox()).is_some() {
            return Ok(Return {
                expr: None,
                segment: start.span,
            });
        }
        let expr = Box::new(self.value()?);
        let segment = start.span.start..expr.segment().end;
        Ok(Return {
            expr: Some(expr),
            segment,
        })
    }
}

impl<'a> Parser<'a> {
    fn parse_fn_return_type(&mut self) -> ParseResult<Option<Type<'a>>> {
        if let Some(token) = self.cursor.advance(of_types(&[
            TokenType::Arrow,
            TokenType::FatArrow,
            TokenType::Colon,
        ])) {
            if token.token_type != TokenType::Arrow {
                self.report_error(self.mk_parse_error(
                    "Return types are denoted using `->`.",
                    token.span,
                    ParseErrorKind::Expected("`->`".to_owned()),
                ));
            }
            self.cursor.advance(blanks()); // consume blanks
            self.parse_type().map(Some)
        } else {
            Ok(None)
        }
    }

    fn parse_fn_parameter(&mut self) -> ParseResult<FunctionParameter<'a>> {
        self.cursor.advance(blanks()); //consume blanks

        let vararg_token = self.cursor.lookahead(
            repeat(
                // skip everything that could compose a type expression
                of_types(&[
                    TokenType::Space,
                    TokenType::NewLine,
                    TokenType::Identifier,
                    TokenType::SquaredLeftBracket,
                    TokenType::SquaredRightBracket,
                ]),
            )
            .then(of_type(TokenType::Vararg)),
        );

        if let Some(vararg_token) = vararg_token {
            let param = self
                .cursor
                .lookahead(not(of_type(TokenType::Vararg)))
                .map(|_| self.parse_type())
                .transpose()
                .map(|t| {
                    let vararg_token_segment = vararg_token.span;
                    let segment = if let Some(t) = &t {
                        t.segment().start..vararg_token_segment.end
                    } else {
                        vararg_token_segment
                    };
                    FunctionParameter::Variadic(t, segment)
                })?;
            self.cursor
                .force(of_type(TokenType::Vararg), "expected '...'")?;
            return Ok(param);
        }

        let current_token = self.cursor.peek();

        if self.cursor.advance(of_type(TokenType::Slf)).is_some() {
            return Ok(FunctionParameter::Slf(current_token.span));
        }

        self.parse_typed_var().map(FunctionParameter::Named)
    }

    fn parse_fn_parameter_list(&mut self) -> ParseResult<Vec<FunctionParameter<'a>>> {
        let (params, _) = self.parse_explicit_list(
            TokenType::RoundedLeftBracket,
            TokenType::RoundedRightBracket,
            "expected start of parameter list",
            "Expected parameter.",
            Parser::parse_fn_parameter,
        )?;
        Ok(params)
    }

    fn parse_fn_declaration_name(&mut self) -> ParseResult<Token> {
        self.cursor
            .advance(like(TokenType::is_valid_function_name))
            .ok_or_else(|| {
                //collect all tokens that could compose the function's name
                let wrong_name_slice = self
                    .cursor
                    .collect(repeat(
                        not(blank().or(eox()).or(of_types(&[
                            TokenType::CurlyLeftBracket,
                            TokenType::SquaredLeftBracket,
                            TokenType::RoundedLeftBracket,
                        ])))
                        .and_then(next()),
                    ))
                    .to_owned();
                if wrong_name_slice.is_empty() {
                    self.mk_parse_error(
                        "function name expected",
                        self.cursor.peek().span,
                        ParseErrorKind::Expected("<function name>".to_string()),
                    )
                } else {
                    let start = wrong_name_slice.first().unwrap().span.start;
                    let end = wrong_name_slice.last().unwrap().span.end;
                    self.mk_parse_error(
                        "function name is invalid.",
                        start..end,
                        ParseErrorKind::InvalidFormat,
                    )
                }
            })
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::Call;
    use ast::function::{FunctionDeclaration, FunctionParameter, Return};
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::r#type::{ParametrizedType, Type, TypeParameter};
    use ast::r#use::InclusionPathItem;
    use ast::value::Literal;
    use ast::variable::{TypedVariable, VarName, VarReference};
    use ast::Expr;
    use context::source::{Source, SourceSegmentHolder};
    use context::str_find::{find_between, find_in, find_in_nth};

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::source::{identifier, identifier_nth, literal, literal_nth};

    #[test]
    fn function_no_name() {
        let errs2 = parse(Source::unknown("fun () -> x = ()")).errors;
        let errs3 = parse(Source::unknown("fun () = ()")).errors;
        let errs4 = parse(Source::unknown("fun [X]() = ()")).errors;
        for errs in [errs2, errs3, errs4] {
            assert_eq!(
                errs,
                vec![ParseError {
                    message: "function name expected".to_string(),
                    position: 4..5,
                    kind: ParseErrorKind::Expected("<function name>".to_string()),
                }]
            );
        }
    }

    #[test]
    fn function_nugget() {
        let errs = parse(Source::unknown("fun")).errors;
        assert_eq!(
            errs,
            vec![ParseError {
                message: "function name expected".to_string(),
                position: 3..3,
                kind: ParseErrorKind::Expected("<function name>".to_string()),
            }]
        );
    }

    #[test]
    fn function_with_return() {
        let source = Source::unknown("fun foo() = return 4 + 5");
        let errs = parse(source).expect("parse fail");
        assert_eq!(
            errs,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: identifier(source.source, "foo"),
                type_parameters: vec![],
                parameters: vec![],
                return_type: None,
                body: Some(Box::new(Expr::Return(Return {
                    expr: Some(Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 4.into(),
                            segment: source.source.find('4').map(|p| p..p + 1).unwrap(),
                        })),
                        op: BinaryOperator::Plus,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 5.into(),
                            segment: source.source.find('5').map(|p| p..p + 1).unwrap(),
                        })),
                    }))),
                    segment: find_between(source.source, "return", "4 + 5")
                }))),
                segment: source.segment()
            })]
        );
    }

    #[test]
    fn early_return() {
        let source = Source::unknown("return");
        let exprs = parse(source).expect("parse fail");
        assert_eq!(
            exprs,
            vec![Expr::Return(Return {
                expr: None,
                segment: source.segment()
            })]
        );
    }

    #[test]
    fn return_string() {
        let source = Source::unknown("return 'foo'");
        let exprs = parse(source).expect("parse fail");
        assert_eq!(
            exprs,
            vec![Expr::Return(Return {
                expr: Some(Box::new(Expr::Literal(Literal {
                    parsed: "foo".into(),
                    segment: find_in(source.source, "'foo'"),
                }))),
                segment: source.segment()
            })]
        );
    }

    #[test]
    fn function_no_params() {
        let src = "fun x = y";
        let errs = parse(Source::unknown(src)).errors;
        assert_eq!(
            errs,
            vec![ParseError {
                message: "expected start of parameter list".to_string(),
                position: src.find('=').map(|i| i..i + 1).unwrap(),
                kind: ParseErrorKind::Expected("(".to_string()),
            }]
        );
    }

    #[test]
    fn function_invalid_name() {
        let src = "fun 78() = ()";
        let errs = parse(Source::unknown(src)).errors;
        assert_eq!(
            errs,
            vec![ParseError {
                message: "function name is invalid.".to_string(),
                position: src.find("78").map(|i| i..i + 2).unwrap(),
                kind: ParseErrorKind::InvalidFormat,
            }]
        );
    }

    #[test]
    fn function_invalid_body() {
        let src = "fun foo() \n {}";
        let errs = parse(Source::unknown(src)).errors;
        assert_eq!(
            errs,
            vec![ParseError {
                message: "expected '='".to_string(),
                position: src.find('{').map(|i| i..i + 1).unwrap(),
                kind: ParseErrorKind::Unexpected,
            }]
        );
    }

    #[test]
    fn functions_with_blanks() {
        let src = Source::unknown("fun test()\n -> Float\n =\n    2.0");
        let ast = parse(src).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: identifier(src.source, "test"),
                type_parameters: vec![],
                parameters: vec![],
                return_type: Some(Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(src.source, "Float"))],
                    params: vec![],
                    segment: find_in_nth(src.source, "Float", 0),
                })),
                body: Some(Box::new(Expr::Literal(Literal {
                    parsed: 2.0.into(),
                    segment: find_in_nth(src.source, "2.0", 0),
                }))),
                segment: src.segment(),
            })]
        );
    }

    #[test]
    fn function_declaration() {
        let source = Source::unknown("fun test[]() = x");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: identifier(source.source, "test"),
                type_parameters: vec![],
                parameters: vec![],
                return_type: None,
                body: Some(Box::new(Expr::Call(Call {
                    arguments: vec![literal(source.source, "x")],
                }))),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn function_declaration_param() {
        let source = Source::unknown("fun test(x) = $x");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: identifier(source.source, "test"),
                type_parameters: vec![],
                parameters: vec![FunctionParameter::Named(TypedVariable {
                    name: identifier(source.source, "x"),
                    ty: None,
                })],
                return_type: None,
                body: Some(Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("x"),
                    segment: find_in(source.source, "$x")
                }))),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn function_declaration_no_body() {
        let source = Source::unknown("fun non_implemented_function(x);");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: identifier(source.source, "non_implemented_function"),
                type_parameters: vec![],
                parameters: vec![FunctionParameter::Named(TypedVariable {
                    name: identifier(source.source, "x"),
                    ty: None,
                })],
                return_type: None,
                body: None,
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn function_declaration_params() {
        let source = Source::unknown("fun test[](self,  x : String  ,  y : Test   ) = x");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: identifier(source.source, "test"),
                type_parameters: vec![],
                parameters: vec![
                    FunctionParameter::Slf(find_in(source.source, "self")),
                    FunctionParameter::Named(TypedVariable {
                        name: identifier(source.source, "x"),
                        ty: Some(Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier(
                                source.source,
                                "String"
                            ))],
                            params: vec![],
                            segment: find_in(source.source, "String")
                        })),
                    }),
                    FunctionParameter::Named(TypedVariable {
                        name: identifier(source.source, "y"),
                        ty: Some(Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier(
                                source.source,
                                "Test"
                            ))],
                            params: vec![],
                            segment: find_in(source.source, "Test")
                        })),
                    }),
                ],
                return_type: None,
                body: Some(Box::new(Expr::Call(Call {
                    arguments: vec![literal_nth(source.source, "x", 1)],
                }))),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn function_declaration_tparams() {
        let source = Source::unknown("fun test[X, Y](  x : X  ,  y : Y   ) = x");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: identifier(source.source, "test"),
                type_parameters: vec![
                    TypeParameter {
                        name: identifier(source.source, "X"),
                        params: Vec::new(),
                        segment: find_in(source.source, "X")
                    },
                    TypeParameter {
                        name: identifier(source.source, "Y"),
                        params: Vec::new(),
                        segment: find_in(source.source, "Y")
                    },
                ],
                parameters: vec![
                    FunctionParameter::Named(TypedVariable {
                        name: identifier(source.source, "x"),
                        ty: Some(Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier_nth(
                                source.source,
                                "X",
                                1
                            ))],
                            params: vec![],
                            segment: find_in_nth(source.source, "X", 1)
                        })),
                    }),
                    FunctionParameter::Named(TypedVariable {
                        name: identifier(source.source, "y"),
                        ty: Some(Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier_nth(
                                source.source,
                                "Y",
                                1
                            ))],
                            params: vec![],
                            segment: find_in_nth(source.source, "Y", 1)
                        })),
                    }),
                ],
                return_type: None,
                body: Some(Box::new(Expr::Call(Call {
                    arguments: vec![literal_nth(source.source, "x", 1)],
                }))),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn function_declaration_vararg() {
        let source = Source::unknown("fun test(X...) = $x");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: identifier(source.source, "test"),
                type_parameters: vec![],
                parameters: vec![FunctionParameter::Variadic(
                    Some(Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source.source, "X"))],
                        params: Vec::new(),
                        segment: find_in(source.source, "X")
                    })),
                    find_in(source.source, "X...")
                )],
                return_type: None,
                body: Some(Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("x"),
                    segment: find_in(source.source, "$x")
                }))),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn function_declaration_vararg_notype() {
        let source = Source::unknown("fun test(x: int, ...) = $x");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: identifier(source.source, "test"),
                type_parameters: vec![],
                parameters: vec![
                    FunctionParameter::Named(TypedVariable {
                        name: identifier(source.source, "x"),
                        ty: Some(Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier(source.source, "int"))],
                            params: Vec::new(),
                            segment: find_in(source.source, "int")
                        })),
                    }),
                    FunctionParameter::Variadic(None, find_in(source.source, "..."))
                ],

                return_type: None,
                body: Some(Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("x"),
                    segment: find_in(source.source, "$x")
                }))),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn function_declaration_complete() {
        let source = Source::unknown("fun test[X, Y](  x : X  ,  y : Y   ) -> X = x");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: identifier(source.source, "test"),
                type_parameters: vec![
                    TypeParameter {
                        name: identifier(source.source, "X"),
                        params: Vec::new(),
                        segment: find_in(source.source, "X")
                    },
                    TypeParameter {
                        name: identifier(source.source, "Y"),
                        params: Vec::new(),
                        segment: find_in(source.source, "Y")
                    },
                ],
                parameters: vec![
                    FunctionParameter::Named(TypedVariable {
                        name: identifier(source.source, "x"),
                        ty: Some(Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier_nth(
                                source.source,
                                "X",
                                1
                            ))],
                            params: vec![],
                            segment: find_in_nth(source.source, "X", 1)
                        })),
                    }),
                    FunctionParameter::Named(TypedVariable {
                        name: identifier(source.source, "y"),
                        ty: Some(Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier_nth(
                                source.source,
                                "Y",
                                1
                            ))],
                            params: vec![],
                            segment: find_in_nth(source.source, "Y", 1)
                        })),
                    }),
                ],
                return_type: Some(Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier_nth(
                        source.source,
                        "X",
                        2
                    ))],
                    params: Vec::new(),
                    segment: find_in_nth(source.source, "X", 2)
                })),
                body: Some(Box::new(Expr::Call(Call {
                    arguments: vec![literal_nth(source.source, "x", 1)],
                }))),
                segment: source.segment()
            })]
        );
    }
}
