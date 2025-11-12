use ast::variable::{Identifier, TypedVariable, VarDeclaration, VarKind};
use ast::Expr;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType;

use crate::err::ParseErrorKind;
use crate::moves::{blanks, of_type, spaces, Move};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    /// Parses a variable declaration.
    pub(crate) fn var_declaration(&mut self) -> ParseResult<Expr> {
        let start = self.cursor.next()?;
        let kind = match start.token_type {
            TokenType::Var => VarKind::Var,
            TokenType::Val => VarKind::Val,
            _ => {
                return self.expected(
                    "expected var or val keywords",
                    ParseErrorKind::Expected("var or val".to_string()),
                )
            }
        };

        let var = self.parse_typed_var()?;
        let mut segment = start.span.start..var.segment().end;

        let initializer = match self
            .cursor
            .advance(spaces().then(of_type(TokenType::Equal)))
        {
            None => None,
            Some(_) => {
                let value = self.value()?;
                segment = segment.start..value.segment().end;
                Some(Box::new(value))
            }
        };

        Ok(Expr::VarDeclaration(VarDeclaration {
            kind,
            var,
            initializer,
            segment,
        }))
    }

    pub(crate) fn parse_typed_var(&mut self) -> ParseResult<TypedVariable> {
        let name = self.cursor.force(
            blanks().then(of_type(TokenType::Identifier)),
            "Expected name.",
        )?;

        let ty = self
            .cursor
            .advance(blanks().then(of_type(TokenType::Colon)))
            .map(|_| self.parse_type())
            .transpose()?;
        Ok(TypedVariable {
            name: Identifier::extract(self.source, name.span),
            ty,
        })
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::Call;
    use ast::group::Block;
    use ast::operation::BinaryOperation;
    use ast::operation::BinaryOperator::Plus;
    use ast::r#type::{ParametrizedType, Type};
    use ast::r#use::InclusionPathItem;
    use ast::value::{Literal, LiteralValue};
    use context::str_find::{find_in, find_in_nth};

    use crate::err::ParseError;
    use crate::parse;
    use crate::source::{identifier, literal};

    use super::*;

    #[test]
    fn val_declaration() {
        let source = "val variable";
        let ast = parse(source).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: identifier(source, "variable"),
                    ty: None,
                },
                initializer: None,
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn val_declaration_with_type() {
        let source = "val variable: Int";
        let ast = parse(source).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: identifier(source, "variable"),
                    ty: Some(Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "Int"))],
                        params: Vec::new(),
                        segment: find_in(source, "Int"),
                    })),
                },
                initializer: None,
                segment: source.segment(),
            })],
        )
    }

    #[test]
    fn val_declaration_with_type_no_colon() {
        let source = "val variable Array";
        let res: ParseResult<_> = parse(source).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "expected end of expression or file".to_owned(),
                position: find_in(source, "Array"),
                kind: ParseErrorKind::Unexpected
            })
        )
    }

    #[test]
    fn val_declaration_inferred() {
        let source = "val variable = 'hello $test'";
        let ast = parse(source).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: identifier(source, "variable"),
                    ty: None,
                },
                initializer: Some(Box::from(literal(source, "'hello $test'"))),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn val_declaration_parenthesis_command() {
        let source = "val x = (echo a)";
        let err = parse(source);
        assert_eq!(
            err.errors,
            vec![
                ParseError {
                    message: "Unexpected word literal".to_string(),
                    position: find_in(source, "echo"),
                    kind: ParseErrorKind::Unexpected,
                },
                ParseError {
                    message: "Unexpected token ')'.".to_string(),
                    position: find_in(source, ")"),
                    kind: ParseErrorKind::Unexpected,
                }
            ]
        )
    }

    #[test]
    fn val_declaration_more_expressions() {
        let source = "val x = (1 + 2\n3+ 1)";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "parenthesis in value expression can only contain one expression"
                    .to_string(),
                position: source.find('\n').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn val_declaration_block_command() {
        let source = "val x = {echo a}";
        let result = parse(source).expect("parse fail");
        assert_eq!(
            result,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: identifier(source, "x"),
                    ty: None,
                },
                initializer: Some(Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal(Literal {
                                parsed: "echo".into(),
                                segment: find_in(source, "echo")
                            }),
                            Expr::Literal(Literal {
                                parsed: "a".into(),
                                segment: find_in_nth(source, "a", 1),
                            }),
                        ],
                    })],
                    segment: find_in(source, "{echo a}")
                }))),
                segment: source.segment(),
            })],
        )
    }

    #[test]
    fn val_declaration_arithmetic_expr() {
        let source = "val variable = 7 + 2";
        let ast = parse(source).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: identifier(source, "variable"),
                    ty: None,
                },
                initializer: Some(Box::from(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: LiteralValue::Int(7),
                        segment: find_in(source, "7")
                    })),
                    op: Plus,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: LiteralValue::Int(2),
                        segment: find_in(source, "2")
                    }))
                }))),
                segment: source.segment(),
            })]
        )
    }
}
