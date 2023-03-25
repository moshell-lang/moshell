use ast::lambda::LambdaDef;
use lexer::token::TokenType::{Colon, Comma, FatArrow, Identifier, RoundedLeftBracket, RoundedRightBracket};
use crate::aspects::expr_list::ExpressionListAspect;
use crate::aspects::var_declaration::VarDeclarationAspect;
use crate::err::ParseErrorKind::Expected;
use crate::moves::{any, blanks, eod, eox, lookahead, MoveOperations, not, of_type, repeat};
use crate::parser::{Parser, ParseResult};

pub trait LambdaDefinitionAspect<'a> {
    fn parse_lambda_definition(&mut self) -> ParseResult<LambdaDef<'a>>;

    ///Tries to determine if the parser is at a lambda definition.
    /// THIS METHOD IS LIMITED
    fn is_at_lambda_def(&self) -> bool;
}

impl<'a> LambdaDefinitionAspect<'a> for Parser<'a> {
    fn parse_lambda_definition(&mut self) -> ParseResult<LambdaDef<'a>> {
        //lookahead if the lambda's definition contains only one typed argument which is not surrounded by parentheses.
        if self.cursor
            .advance(
                blanks()
                    .then(not(of_type(RoundedRightBracket)))
                    .then(blanks().then(of_type(Identifier)))
                    .then(blanks()).then(of_type(Colon))
            )
            .is_some() {
            return self.expected(
                "Please surround typed argument with parentheses",
                Expected("Surround expression with parenthesis".to_owned()),
            )
        }

        let args = self.parse_implicit_list(
            RoundedLeftBracket, RoundedRightBracket,
            false, Self::parse_typed_var,
        )?;
        self.cursor.force_with(
            blanks().then(of_type(FatArrow)),
            "expected lambda arrow",
            Expected("=>".to_string()),
        )?;
        let body = Box::new(self.value()?);
        Ok(LambdaDef {
            args,
            body,
        })
    }

    fn is_at_lambda_def(&self) -> bool {
        self.cursor.lookahead(
            //is it like "<identifier> =>" ?
            blanks()
                .then(of_type(Identifier))
                //or is it like "(<identifier>, <identifier>, ...) =>" ?
                .or(
                    blanks().then(of_type(RoundedLeftBracket))
                        .and_then(
                            repeat(
                                blanks().then(any())
                                    .then(blanks())
                                    .then(of_type(Comma)
                                        .or(lookahead(eod().or(eox()))))
                            ).and_then(eod())
                        )
                )
                .and_then(blanks().then(of_type(FatArrow)))
        ).is_some()
    }
}

#[cfg(test)]
mod tests {
    use ast::Expr;
    use ast::lambda::LambdaDef;
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::variable::{TypedVariable, VarReference};
    use context::source::Source;
    use crate::aspects::lambda_def::LambdaDefinitionAspect;
    use crate::parser::Parser;
    use pretty_assertions::assert_eq;
    use ast::call::Call;
    use ast::group::Block;
    use ast::r#type::{SimpleType, Type};
    use crate::err::{ParseError, ParseErrorKind};
    use crate::err::ParseErrorKind::Unexpected;

    #[test]
    fn lambda_detection() {
        assert!(Parser::new(Source::unknown("(a) => $a + $b")).is_at_lambda_def());
        assert!(Parser::new(Source::unknown("a => $a + $b")).is_at_lambda_def());
        assert!(Parser::new(Source::unknown("() => $a + $b")).is_at_lambda_def());
        assert!(Parser::new(Source::unknown("(a, b) => $a + $b")).is_at_lambda_def());
    }

    #[test]
    fn simple_lambda_definition() {
        let source = Source::unknown("(a, b: Int) => $a + $b");
        let parsed = Parser::new(source).parse_lambda_definition().expect("Failed to parse.");
        assert_eq!(
            parsed,
            LambdaDef {
                args: vec![
                    TypedVariable {
                        name: "a",
                        ty: None,
                    },
                    TypedVariable {
                        name: "b",
                        ty: Some(Type::Simple(SimpleType {
                            name: "Int",
                            params: Vec::new(),
                        })),
                    },
                ],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: "a"
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: "b"
                    })),
                })),
            }
        );
    }


    #[test]
    fn simple_lambda_definition_one_arg() {
        let source = Source::unknown("a => $a + $b");
        let parsed = Parser::new(source).parse_lambda_definition().expect("Failed to parse.");
        assert_eq!(
            parsed,
            LambdaDef {
                args: vec![
                    TypedVariable {
                        name: "a",
                        ty: None,
                    },
                ],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: "a"
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: "b"
                    })),
                })),
            }
        );
    }

    #[test]
    fn simple_lambda_definition_one_arg_typed() {
        let src = "a: Int => $a + $b";
        let source = Source::unknown(src);
        let parsed = Parser::new(source).parse_lambda_definition().expect_err("parse did not fail");
        assert_eq!(
            parsed,
            ParseError {
                message: "Please surround typed argument with parentheses".to_string(),
                kind: ParseErrorKind::Expected("Surround expression with parenthesis".to_string()),
                position: src.find(':').map(|i| i + 1..i + 2).unwrap(),
            }
        );
    }

    #[test]
    fn simple_lambda_definition_one_arg_typed_wrapped() {
        let src = "(a: Int) => $a + $b";
        let source = Source::unknown(src);
        let parsed = Parser::new(source).parse_lambda_definition().expect("parse fail");
        assert_eq!(
            parsed,
            LambdaDef {
                args: vec![
                    TypedVariable {
                        name: "a",
                        ty: Some(Type::Simple(SimpleType {
                            name: "Int",
                            params: Vec::new(),
                        })),
                    },
                ],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: "a"
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: "b"
                    })),
                })),
            }
        );
    }

    #[test]
    fn simple_lambda_definition_emptyargs() {
        let source = Source::unknown("() => {echo hey}");
        let parsed = Parser::new(source).parse_lambda_definition().expect("Failed to parse.");
        assert_eq!(
            parsed,
            LambdaDef {
                args: Vec::new(),
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal("echo".into()),
                            Expr::Literal("hey".into()),
                        ],
                        type_parameters: Vec::new(),
                    })]
                })),
            }
        );
    }

    #[test]
    fn simple_lambda_definition_noargs() {
        let source = Source::unknown(" => {echo hey}");
        let parsed = Parser::new(source).parse_lambda_definition().expect_err("parser did not fail");
        assert_eq!(
            parsed,
            ParseError {
                message: "Expected name.".to_string(),
                position: 1..3,
                kind: Unexpected,
            }
        );
    }
}