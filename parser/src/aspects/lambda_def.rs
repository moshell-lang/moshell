use ast::lambda::LambdaDef;
use lexer::token::TokenType::{FatArrow, RoundedLeftBracket, RoundedRightBracket};
use crate::aspects::expr_list::ExpressionListAspect;
use crate::aspects::var_declaration::VarDeclarationAspect;
use crate::err::ParseErrorKind::Expected;
use crate::moves::{blanks, MoveOperations, of_type};
use crate::parser::{Parser, ParseResult};

pub trait LambdaDefinitionAspect<'a> {
    fn parse_lambda_definition(&mut self) -> ParseResult<LambdaDef<'a>>;

}

impl<'a> LambdaDefinitionAspect<'a> for Parser<'a> {
    fn parse_lambda_definition(&mut self) -> ParseResult<LambdaDef<'a>> {
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

}

impl<'a> Parser<'a> {

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
    use crate::err::{ParseError};
    use crate::err::ParseErrorKind::Unexpected;

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