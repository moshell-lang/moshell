use lexer::token::TokenType;
use lexer::token::TokenType::{Else, NewLine, SemiColon};
use crate::ast::Expr;
use crate::moves::{MoveOperations, of_type, repeat, spaces};
use crate::parser::{Parser, ParseResult};
use crate::ast::flow_control::If;
pub trait IfElseAspect<'a> {
    fn parse_if(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> IfElseAspect<'a> for Parser<'a> {
    fn parse_if(&mut self) -> ParseResult<Expr<'a>> {
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
        let success_branch = self.next_statement()?;

        let fail_branch =
            if self.cursor.advance(
                repeat(spaces().pipe(of_type(NewLine)))
                    .then(of_type(SemiColon))
                    .then(repeat(spaces().pipe(of_type(NewLine))))
                    .then(of_type(Else))
            ).is_some() {
                Some(self.next_statement()?)
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
    use crate::ast::test::Test;
    use crate::ast::variable::VarReference;
    use crate::parse;

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

}