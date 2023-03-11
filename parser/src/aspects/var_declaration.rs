use lexer::token::TokenType;

use crate::err::ParseErrorKind;
use ast::variable::{TypedVariable, VarDeclaration, VarKind};
use ast::Expr;

use crate::moves::{of_type, of_types, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};

pub trait VarDeclarationAspect<'a> {
    /// Parses a variable declaration.
    fn var_declaration(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> VarDeclarationAspect<'a> for Parser<'a> {
    /// Parses a variable declaration.
    fn var_declaration(&mut self) -> ParseResult<Expr<'a>> {
        let kind = match self.cursor.next()?.token_type {
            TokenType::Var => VarKind::Var,
            TokenType::Val => VarKind::Val,
            _ => {
                return self.expected(
                    "expected var or val keywords",
                    ParseErrorKind::Excepted("var or val"),
                )
            }
        };
        let name = self
            .cursor
            .force(
                spaces().and_then(of_type(TokenType::Identifier)),
                "Expected variable name.",
            )?
            .value;

        let ty = match self
            .cursor
            .advance(spaces().then(of_type(TokenType::Colon)))
        {
            None => None,
            Some(_) => Some(
                self.cursor
                    .force(
                        spaces().then(of_type(TokenType::Identifier)),
                        "Expected identifier for variable type",
                    )?
                    .value,
            ),
        };
        let initializer = match self
            .cursor
            .advance(spaces().then(of_type(TokenType::Equal)))
        {
            None => {
                self.cursor.force(
                    of_types(&[TokenType::NewLine, TokenType::EndOfFile]),
                    "Expected newline after variable declaration",
                )?;
                None
            }

            Some(_) => Some(self.value()?),
        };

        Ok(Expr::VarDeclaration(VarDeclaration {
            kind,
            var: TypedVariable { name, ty },
            initializer: initializer.map(Box::new),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::err::ParseError;
    use crate::parser::Parser;
    use ast::callable::Call;
    use ast::group::Block;
    use ast::operation::BinaryOperation;
    use ast::operation::BinaryOperator::Plus;
    use ast::value::{Literal, LiteralValue};
    use ast::Expr;
    use context::source::Source;
    use pretty_assertions::assert_eq;

    #[test]
    fn val_declaration() {
        let source = Source::unknown("val variable");
        let ast = Parser::new(source)
            .var_declaration()
            .expect("failed to parse");
        assert_eq!(
            ast,
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "variable",
                    ty: None
                },
                initializer: None,
            })
        )
    }

    #[test]
    fn val_declaration_with_type() {
        let source = Source::unknown("val variable: Array");
        let ast = Parser::new(source)
            .var_declaration()
            .expect("failed to parse");
        assert_eq!(
            ast,
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "variable",
                    ty: Some("Array"),
                },
                initializer: None,
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
        let ast = Parser::new(source)
            .var_declaration()
            .expect("failed to parse");
        assert_eq!(
            ast,
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "variable",
                    ty: None,
                },
                initializer: Some(Box::from(Expr::Literal(Literal {
                    lexeme: "'hello $test'",
                    parsed: "hello $test".into(),
                }))),
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
        let result = Parser::new(source).var_declaration().expect("parse fail");
        assert_eq!(
            result,
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "x",
                    ty: None,
                },
                initializer: Some(Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal(Literal {
                                lexeme: "echo",
                                parsed: "echo".into(),
                            }),
                            Expr::Literal(Literal {
                                lexeme: "a",
                                parsed: "a".into(),
                            }),
                        ],
                        tparams: vec![],
                    })]
                }))),
            })
        )
    }

    #[test]
    fn val_declaration_arithmetic_expr() {
        let source = Source::unknown("val variable = 7 + 2");
        let ast = Parser::new(source)
            .var_declaration()
            .expect("failed to parse");
        assert_eq!(
            ast,
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "variable",
                    ty: None,
                },
                initializer: Some(Box::from(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        lexeme: "7",
                        parsed: LiteralValue::Int(7)
                    })),
                    op: Plus,
                    right: Box::new(Expr::Literal(Literal {
                        lexeme: "2",
                        parsed: LiteralValue::Int(2)
                    }))
                }))),
            })
        )
    }
}
