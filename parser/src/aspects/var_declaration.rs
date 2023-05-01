use crate::aspects::r#type::TypeAspect;
use ast::variable::{TypedVariable, VarDeclaration, VarKind};
use ast::Expr;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType;

use crate::err::ParseErrorKind;
use crate::moves::{blanks, eod, eox, lookahead, of_type, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};

pub trait VarDeclarationAspect<'a> {
    /// Parses a variable declaration.
    fn var_declaration(&mut self) -> ParseResult<Expr<'a>>;

    /// Parses a typed var,
    fn parse_typed_var(&mut self) -> ParseResult<TypedVariable<'a>>;
}

impl<'a> VarDeclarationAspect<'a> for Parser<'a> {
    /// Parses a variable declaration.
    fn var_declaration(&mut self) -> ParseResult<Expr<'a>> {
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
        let mut segment = self.cursor.relative_pos_ctx(start).start..var.segment.end;

        let initializer = match self
            .cursor
            .advance(spaces().then(of_type(TokenType::Equal)))
        {
            None => {
                self.cursor.force(
                    spaces().then(eox().or(lookahead(eod()))),
                    "Expected initializer after declaration or newline.",
                )?;
                None
            }

            Some(_) => {
                let value = self.value()?;
                segment = segment.start..value.segment().end;
                Some(value)
            }
        }
        .map(Box::new);

        Ok(Expr::VarDeclaration(VarDeclaration {
            kind,
            var,
            initializer,
            segment,
        }))
    }

    fn parse_typed_var(&mut self) -> ParseResult<TypedVariable<'a>> {
        let name = self.cursor.force(
            blanks().then(of_type(TokenType::Identifier)),
            "Expected name.",
        )?;

        let ty = self
            .cursor
            .advance(blanks().then(of_type(TokenType::Colon)))
            .map(|_| self.parse_type())
            .transpose()?;
        let mut segment = self.cursor.relative_pos_ctx(name.value);
        if let Some(ty) = ty.as_ref() {
            segment = segment.start..ty.segment().end;
        }
        Ok(TypedVariable {
            name: name.value,
            ty,
            segment,
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
    use ast::value::{Literal, LiteralValue};
    use ast::Expr;
    use context::source::{Source, SourceSegmentHolder};
    use context::str_find::{find_in, find_in_nth};

    use crate::err::ParseError;
    use crate::parse;
    use crate::parser::Parser;
    use crate::source::literal;

    use super::*;

    #[test]
    fn val_declaration() {
        let source = Source::unknown("val variable");
        let ast = parse(source.clone()).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "variable",
                    ty: None,
                    segment: find_in(&source.source, "variable")
                },
                initializer: None,
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn val_declaration_with_type() {
        let source = Source::unknown("val variable: Int");
        let ast = parse(source.clone()).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "variable",
                    ty: Some(Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "Int",
                        params: Vec::new(),
                        segment: find_in(&source.source, "Int"),
                    })),
                    segment: find_in(&source.source, "variable: Int")
                },
                initializer: None,
                segment: source.segment(),
            })],
        )
    }

    #[test]
    fn val_declaration_with_type_no_colon() {
        let source = Source::unknown("val variable Array");
        Parser::new(source)
            .var_declaration()
            .expect_err("did not fail");
    }

    #[test]
    fn val_declaration_inferred() {
        let source = Source::unknown("val variable = 'hello $test'");
        let ast = parse(source.clone()).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "variable",
                    ty: None,
                    segment: find_in(source.source, "variable")
                },
                initializer: Some(Box::from(literal(source.source, "'hello $test'"))),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn val_declaration_parenthesis_command() {
        let content = "val x = (echo a)";
        let source = Source::unknown(content);
        let err = parse(source);
        assert_eq!(
            err.errors,
            vec![ParseError {
                message: "invalid infix operator".to_string(),
                position: content.rfind('a').map(|p| (p..p + 1)).unwrap(),
                kind: ParseErrorKind::Unexpected,
            }]
        )
    }

    #[test]
    fn val_declaration_more_expressions() {
        let content = "val x = (1 + 2\n3+ 1)";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "parenthesis in value expression can only contain one expression"
                    .to_string(),
                position: content.find('\n').map(|p| (p..p + 1)).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn val_declaration_block_command() {
        let source = Source::unknown("val x = {echo a}");
        let result = parse(source.clone()).expect("parse fail");
        assert_eq!(
            result,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "x",
                    ty: None,
                    segment: find_in(&source.source, "x")
                },
                initializer: Some(Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        path: Vec::new(),
                        arguments: vec![
                            Expr::Literal(Literal {
                                parsed: "echo".into(),
                                segment: find_in(&source.source, "echo")
                            }),
                            Expr::Literal(Literal {
                                parsed: "a".into(),
                                segment: find_in_nth(&source.source, "a", 1),
                            }),
                        ],
                        type_parameters: vec![],
                    })],
                    segment: find_in(&source.source, "{echo a}")
                }))),
                segment: source.segment(),
            })],
        )
    }

    #[test]
    fn val_declaration_arithmetic_expr() {
        let source = Source::unknown("val variable = 7 + 2");
        let ast = parse(source.clone()).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "variable",
                    ty: None,
                    segment: find_in(source.source, "variable")
                },
                initializer: Some(Box::from(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: LiteralValue::Int(7),
                        segment: find_in(source.source, "7")
                    })),
                    op: Plus,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: LiteralValue::Int(2),
                        segment: find_in(source.source, "2")
                    }))
                }))),
                segment: source.segment(),
            })]
        )
    }
}
