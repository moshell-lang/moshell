use lexer::token::TokenType;
use lexer::token::TokenType::{Else, NewLine, SemiColon};
use crate::ast::Expr;
use crate::moves::{MoveOperations, of_type, repeat, spaces};
use crate::parser::{Parser, ParseResult};
use crate::ast::flow_control::If;
pub trait IfElseAspect<'a> {
    fn parse_if<F>(&mut self, parse_branch: F) -> ParseResult<Expr<'a>>
        where F: FnMut(&mut Self) -> ParseResult<Expr<'a>>;
}

impl<'a> IfElseAspect<'a> for Parser<'a> {
    fn parse_if<F>(&mut self, mut parse_branch: F) -> ParseResult<Expr<'a>>
        where F: FnMut(&mut Self) -> ParseResult<Expr<'a>> {
        self.cursor.force(
            of_type(TokenType::If),
            "expected 'if' at start of if expression",
        )?;
        let condition = self.expression_statement()?;

        //skip only one semicolon if any, surrounded by newlines and spaces
        self.cursor.advance(spaces().then(
            repeat(spaces().pipe(of_type(NewLine)))
                .then(of_type(SemiColon))
                .then(repeat(spaces().pipe(of_type(NewLine))))
        ));

        if self.cursor.lookahead(spaces().then(of_type(SemiColon))).is_some() {
            //if the if condition is followed by at least two semicolon, then the if body is invalid
            return self.expected("Forbidden ';' expression after if condition")
        }

        //the success_branch of the if
        let success_branch = parse_branch(self)?;

        let fail_branch =
            if self.cursor.advance(
                repeat(spaces().pipe(of_type(NewLine)))
                    .then(of_type(SemiColon))
                    .then(repeat(spaces().pipe(of_type(NewLine))))
                    .then(of_type(Else))
            ).is_some() {
                Some(parse_branch(self)?)
            } else {
                None
            };

        Ok(Expr::If(If {
            condition: Box::new(condition),
            success_branch: Box::new(success_branch),
            fail_branch: Box::new(fail_branch),
        }))
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use lexer::lexer::lex;
    use crate::ast::callable::Call;
    use crate::ast::Expr;
    use crate::ast::flow_control::If;
    use crate::ast::group::Block;
    use crate::ast::literal::Literal;
    use crate::ast::operation::{BinaryOperation, BinaryOperator};
    use crate::ast::test::Test;
    use crate::ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
    use crate::parse;
    use crate::parser::ParseError;

    #[test]
    fn simple_if() {
        let ast = parse(lex("if [ $1 ]; echo test")).expect("parse failed");
        assert_eq!(
            ast,
            vec![
                Expr::If(If {
                    condition: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::VarReference(VarReference {
                            name: "1"
                        }))
                    })),
                    success_branch: Box::new(Expr::Call(Call {
                        arguments: vec![Expr::Literal("echo".into()), Expr::Literal("test".into())]
                    })),
                    fail_branch: Box::new(None),
                })
            ]
        )
    }


    #[test]
    fn if_else_if() {
        let ast = parse(lex("if [ $1 ]; echo test\n\n\nelse if [ $a ] \n;\n { $7 }; else $5")).expect("parse failed");
        assert_eq!(
            ast,
            vec![
                Expr::If(If {
                    condition: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::VarReference(VarReference {
                            name: "1"
                        }))
                    })),
                    success_branch: Box::new(Expr::Call(Call {
                        arguments: vec![Expr::Literal("echo".into()), Expr::Literal("test".into())]
                    })),
                    fail_branch: Box::new(Some(Expr::If(If {
                        condition: Box::new(Expr::Test(Test {
                            expression: Box::new(Expr::VarReference(VarReference {
                                name: "a"
                            }))
                        })),
                        success_branch: Box::new(Expr::Block(Block {
                            expressions: vec![Expr::VarReference(VarReference {
                                name: "7"
                            })]
                        })),
                        fail_branch: Box::new(Some(Expr::VarReference(VarReference {
                            name: "5"
                        }))),
                    }))),
                })
            ]
        )
    }

    #[test]
    fn if_else_if_separations() {
        let ast = parse(lex("if [ $1 ]; echo test; else if [ $a ]; $7 else $5")).expect("parse failed");
        assert_eq!(
            ast,
            vec![
                Expr::If(If {
                    condition: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::VarReference(VarReference {
                            name: "1"
                        }))
                    })),
                    success_branch: Box::new(Expr::Call(Call {
                        arguments: vec![Expr::Literal("echo".into()), Expr::Literal("test".into())]
                    })),
                    fail_branch: Box::new(Some(Expr::If(If {
                        condition: Box::new(Expr::Test(Test {
                            expression: Box::new(Expr::VarReference(VarReference {
                                name: "a"
                            }))
                        })),
                        success_branch: Box::new(Expr::VarReference(VarReference {
                            name: "7"
                        })),
                        fail_branch: Box::new(Some(Expr::VarReference(VarReference {
                            name: "5"
                        }))),
                    }))),
                })
            ]
        )
    }

    #[test]
    fn if_else_as_value() {
        let ast = parse(lex("val x = if [ $1 ]; 1 + 8 * 7 else if [ $a ]; 2 == 2 else $a && b")).expect("parse failed");
        assert_eq!(
            ast,
            vec![
                Expr::VarDeclaration(VarDeclaration {
                    kind: VarKind::Val,
                    var: TypedVariable {
                        name: "x",
                        ty: None,
                    },
                    initializer: Some(Box::new(Expr::If(If {
                        condition: Box::new(Expr::Test(Test {
                            expression: Box::new(Expr::VarReference(VarReference {
                                name: "1"
                            }))
                        })),
                        success_branch: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                lexeme: "1",
                                parsed: 1.into(),
                            })),
                            op: BinaryOperator::Plus,
                            right: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    lexeme: "8",
                                    parsed: 8.into(),
                                })),
                                op: BinaryOperator::Times,
                                right: Box::new(Expr::Literal(Literal {
                                    lexeme: "7",
                                    parsed: 7.into(),
                                })),
                            })),
                        })),
                        fail_branch: Box::new(Some(Expr::If(If {
                            condition: Box::new(Expr::Test(Test {
                                expression: Box::new(Expr::VarReference(VarReference {
                                    name: "a"
                                }))
                            })),
                            success_branch: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    lexeme: "2",
                                    parsed: 2.into(),
                                })),
                                op: BinaryOperator::EqualEqual,
                                right: Box::new(Expr::Literal(Literal {
                                    lexeme: "2",
                                    parsed: 2.into(),
                                })),
                            })),
                            fail_branch: Box::new(Some(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::VarReference(VarReference {
                                    name: "a"
                                })),
                                op: BinaryOperator::And,
                                right: Box::new(Expr::Literal("b".into())),
                            }))),
                        }))),
                    }))),
                }),
            ]
        )
    }

    #[test]
    fn if_else_bad_brackets() {
        let ast = parse(lex("val x = if [ $1 ] \n { echo hey; else if [ $a ]; echo hola; else echo bonjour }"));
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Unexpected keyword 'else'".to_string()
            })
        )
    }
}