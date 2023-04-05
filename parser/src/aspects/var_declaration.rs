use crate::aspects::r#type::TypeAspect;
use ast::variable::{TypedVariable, VarDeclaration, VarKind};
use ast::Expr;
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

            Some(_) => Some(self.value()?),
        }
        .map(Box::new);

        Ok(Expr::VarDeclaration(VarDeclaration {
            kind,
            var,
            initializer,
            segment: self.cursor.relative_pos_ctx(start..self.cursor.peek()),
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
        Ok(TypedVariable {
            name: name.value,
            ty,
            segment: self.cursor.relative_pos_ctx(name..self.cursor.peek()),
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
    use ast::r#type::{SimpleType, Type};
    use ast::value::{Literal, LiteralValue};
    use ast::Expr;
    use context::source::{Source, SourceSegmentHolder};

    use crate::err::{find_in, ParseError};
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn val_declaration() {
        let source = Source::unknown("val variable");
        let ast = Parser::new(source.clone())
            .var_declaration()
            .expect("failed to parse");
        assert_eq!(
            ast,
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "variable",
                    ty: None,
                    segment: find_in(&source.source, "variable")
                },
                initializer: None,
                segment: source.segment(),
            })
        )
    }

    #[test]
    fn val_declaration_with_type() {
        let source = Source::unknown("val variable: Int");
        let ast = Parser::new(source.clone())
            .var_declaration()
            .expect("failed to parse");
        assert_eq!(
            ast,
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "variable",
                    ty: Some(Type::Simple(SimpleType {
                        name: "Int",
                        params: Vec::new(),
                        segment: find_in(&source.source, "Int"),
                    })),
                    segment: find_in(&source.source, "variable")
                },
                initializer: None,
                segment: source.segment(),
            })
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
        let ast = Parser::new(source.clone())
            .var_declaration()
            .expect("failed to parse");
        assert_eq!(
            ast,
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "variable",
                    ty: None,
                    segment: find_in(&source.source, "variable")
                },
                initializer: Some(Box::from(Expr::Literal(Literal {
                    parsed: "hello $test".into(),
                    segment: find_in(&source.source, "variable")
                }))),
                segment: 0..source.source.len(),
            })
        )
    }

    #[test]
    fn val_declaration_parenthesis_command() {
        let content = "val x = (echo a)";
        let source = Source::unknown(content);
        let err = Parser::new(source).var_declaration();
        assert_eq!(
            err,
            Err(ParseError {
                message: "invalid infix operator".to_string(),
                position: content.rfind('a').map(|p| (p..p + 1)).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn val_declaration_block_command() {
        let source = Source::unknown("val x = {echo a}");
        let result = Parser::new(source.clone())
            .var_declaration()
            .expect("parse fail");
        assert_eq!(
            result,
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "x",
                    ty: None,
                    segment: find_in(&source.source, "x")
                },
                initializer: Some(Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal(Literal {
                                parsed: "echo".into(),
                                segment: find_in(&source.source, "echo")
                            }),
                            Expr::Literal(Literal {
                                parsed: "a".into(),
                                segment: find_in(&source.source, "a"),
                            }),
                        ],
                        type_parameters: vec![],
                    })],
                    segment: find_in(&source.source, "{echo a}")
                }))),
                segment: source.segment(),
            })
        )
    }

    #[test]
    fn val_declaration_arithmetic_expr() {
        let source = Source::unknown("val variable = 7 + 2");
        let ast = Parser::new(source.clone())
            .var_declaration()
            .expect("failed to parse");
        assert_eq!(
            ast,
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "variable",
                    ty: None,
                    segment: find_in(&source.source, "variable")
                },
                initializer: Some(Box::from(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: LiteralValue::Int(7),
                        segment: find_in(&source.source, "7")
                    })),
                    op: Plus,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: LiteralValue::Int(2),
                        segment: find_in(&source.source, "2")
                    }))
                }))),
                segment: 0..source.source.len(),
            })
        )
    }
}
