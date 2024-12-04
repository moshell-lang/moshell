use ast::lambda::LambdaDef;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType::{FatArrow, RoundedLeftBracket, RoundedRightBracket};

use crate::err::ParseErrorKind::Expected;
use crate::moves::{blanks, of_type, Move};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    /// Parses a lambda definition (ex: `(a) => $b + $a`)
    pub(crate) fn parse_lambda_definition(&mut self) -> ParseResult<LambdaDef> {
        let (args, mut segment) = self.parse_implicit_list(
            RoundedLeftBracket,
            RoundedRightBracket,
            "Expected lambda parameter.",
            Self::parse_typed_var,
        )?;
        self.cursor.force_with(
            blanks().then(of_type(FatArrow)),
            "expected lambda arrow",
            Expected("=>".to_string()),
        )?;
        let body = Box::new(self.value()?);
        segment.end = body.segment().end;
        Ok(LambdaDef {
            args,
            body,
            segment,
        })
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::Call;
    use ast::group::Block;
    use ast::lambda::LambdaDef;
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::r#type::{ParametrizedType, Type};
    use ast::r#use::InclusionPathItem;
    use ast::variable::{TypedVariable, VarName, VarReference};
    use ast::Expr;
    use context::source::SourceSegmentHolder;
    use context::str_find::{find_between, find_in};

    use crate::err::ParseError;
    use crate::err::ParseErrorKind::Unexpected;
    use crate::parser::Parser;
    use crate::source::{identifier, literal};

    #[test]
    fn simple_lambda_definition() {
        let source = "(a, b: Int) => $a + $b";
        let parsed = Parser::new(source)
            .parse_lambda_definition()
            .expect("Failed to parse.");
        assert_eq!(
            parsed,
            LambdaDef {
                args: vec![
                    TypedVariable {
                        name: identifier(source, "a"),
                        ty: None,
                    },
                    TypedVariable {
                        name: identifier(source, "b"),
                        ty: Some(Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier(source, "Int"))],
                            params: Vec::new(),
                            segment: find_in(source, "Int"),
                        })),
                    },
                ],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("a".into()),
                        segment: find_in(source, "$a"),
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("b".into()),
                        segment: find_in(source, "$b")
                    })),
                })),
                segment: source.segment()
            }
        );
    }

    #[test]
    fn simple_lambda_definition_one_arg() {
        let source = "a => $a + $b";
        let parsed = Parser::new(source)
            .parse_lambda_definition()
            .expect("Failed to parse.");
        assert_eq!(
            parsed,
            LambdaDef {
                args: vec![TypedVariable {
                    name: identifier(source, "a"),
                    ty: None,
                },],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("a".into()),
                        segment: find_in(source, "$a")
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("b".into()),
                        segment: find_in(source, "$b")
                    })),
                })),
                segment: source.segment()
            }
        );
    }

    #[test]
    fn simple_lambda_definition_one_arg_typed_wrapped() {
        let source = "(a: Int) => $a + $b";
        let parsed = Parser::new(source)
            .parse_lambda_definition()
            .expect("parse fail");
        assert_eq!(
            parsed,
            LambdaDef {
                args: vec![TypedVariable {
                    name: identifier(source, "a"),
                    ty: Some(Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "Int"))],
                        params: Vec::new(),
                        segment: find_in(source, "Int")
                    })),
                },],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("a".into()),
                        segment: find_in(source, "$a")
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("b".into()),
                        segment: find_in(source, "$b")
                    })),
                })),
                segment: source.segment()
            }
        );
    }

    #[test]
    fn simple_lambda_definition_emptyargs() {
        let source = "() => {echo hey}";
        let parsed = Parser::new(source)
            .parse_lambda_definition()
            .expect("Failed to parse.");
        assert_eq!(
            parsed,
            LambdaDef {
                args: Vec::new(),
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![literal(source, "echo"), literal(source, "hey")],
                    })],
                    segment: find_between(source, "{", "}")
                })),
                segment: source.segment()
            }
        );
    }

    #[test]
    fn simple_lambda_definition_noargs() {
        let source = " => {echo hey}";
        let parsed = Parser::new(source)
            .parse_lambda_definition()
            .expect_err("parser did not fail");
        assert_eq!(
            parsed,
            ParseError {
                message: "Expected name.".to_string(),
                position: find_in(source, "=>"),
                kind: Unexpected,
            }
        );
    }
}
