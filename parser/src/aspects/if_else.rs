use crate::ast::control_flow::If;
use crate::ast::Expr;
use crate::moves::{aerated, blanks, of_type, MoveOperations};
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType;
use lexer::token::TokenType::{Else, SemiColon};

///parser aspect for if and else expressions.
pub trait IfElseAspect<'a> {
    ///parse a if with optional else expression.
    /// `parse_branch` argument defines how the branches must be parsed.
    fn parse_if<F>(&mut self, parse_branch: F) -> ParseResult<Expr<'a>>
    where
        F: FnMut(&mut Self) -> ParseResult<Expr<'a>>;
}

impl<'a> IfElseAspect<'a> for Parser<'a> {
    fn parse_if<F>(&mut self, mut parse_branch: F) -> ParseResult<Expr<'a>>
    where
        F: FnMut(&mut Self) -> ParseResult<Expr<'a>>,
    {
        self.cursor.force(
            of_type(TokenType::If),
            "expected 'if' at start of if expression",
        )?;
        let condition = self.expression_statement()?;

        //skip only one semicolon if any, surrounded by newlines and spaces
        self.cursor
            .advance(aerated(of_type(SemiColon)).or(blanks()));

        //the success_branch of the if
        let success_branch = parse_branch(self)?;

        //parse the 'else' branch.
        let fail_branch = if self
            .cursor
            .advance(
                blanks()
                    .then(of_type(SemiColon))
                    .then(aerated(of_type(Else))),
            )
            .is_some()
        {
            Some(Box::new(parse_branch(self)?))
        } else {
            None
        };

        Ok(Expr::If(If {
            condition: Box::new(condition),
            success_branch: Box::new(success_branch),
            fail_branch,
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::callable::Call;
    use crate::ast::control_flow::If;
    use crate::ast::group::Block;
    use crate::ast::operation::BinaryOperator::And;
    use crate::ast::operation::{BinaryOperation, BinaryOperator};
    use crate::ast::test::Test;
    use crate::ast::value::{Literal, TemplateString};
    use crate::ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
    use crate::ast::Expr;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::ParseResult;
    use context::source::Source;
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_if() {
        let source = Source::unknown("if [ $1 ]; echo test");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::If(If {
                condition: Box::new(Expr::Test(Test {
                    expression: Box::new(Expr::VarReference(VarReference { name: "1" }))
                })),
                success_branch: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("echo".into()), Expr::Literal("test".into())]
                })),
                fail_branch: None,
            })]
        )
    }

    #[test]
    fn if_else_if() {
        let source = Source::unknown(
            "if echo a && [[ -f /file/exe ]]; echo test\n\n\nelse if [ $a ] \n;\n { $7 }; else $5",
        );
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::If(If {
                condition: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Call(Call {
                        arguments: vec![Expr::Literal("echo".into()), Expr::Literal("a".into())]
                    })),
                    op: And,
                    right: Box::new(Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal("test".into()),
                            Expr::Literal("-f".into()),
                            Expr::Literal("/file/exe".into())
                        ]
                    }))
                })),
                success_branch: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("echo".into()), Expr::Literal("test".into())]
                })),
                fail_branch: Some(Box::new(Expr::If(If {
                    condition: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::VarReference(VarReference { name: "a" }))
                    })),
                    success_branch: Box::new(Expr::Block(Block {
                        expressions: vec![Expr::VarReference(VarReference { name: "7" })]
                    })),
                    fail_branch: Some(Box::new(Expr::VarReference(VarReference { name: "5" }))),
                }))),
            })]
        )
    }

    #[test]
    fn if_else_if_separations() {
        let source = Source::unknown("if [ $1 ]; echo test; else if [ $a ]; $7 else $5");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::If(If {
                condition: Box::new(Expr::Test(Test {
                    expression: Box::new(Expr::VarReference(VarReference { name: "1" }))
                })),
                success_branch: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("echo".into()), Expr::Literal("test".into())]
                })),
                fail_branch: Some(Box::new(Expr::If(If {
                    condition: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::VarReference(VarReference { name: "a" }))
                    })),
                    success_branch: Box::new(Expr::VarReference(VarReference { name: "7" })),
                    fail_branch: Some(Box::new(Expr::VarReference(VarReference { name: "5" }))),
                }))),
            })]
        )
    }

    #[test]
    fn no_separation_else() {
        let source = Source::unknown("if $x {} else {}");
        let ast = parse(source).expect("parse fail");
        assert_eq!(
            ast,
            vec![Expr::If(If {
                condition: Box::new(Expr::VarReference(VarReference { name: "x" })),
                success_branch: Box::new(Expr::Block(Block {
                    expressions: vec![]
                })),
                fail_branch: Some(Box::new(Expr::Block(Block {
                    expressions: vec![]
                })))
            })]
        )
    }

    #[test]
    fn if_else_as_value() {
        let source =
            Source::unknown("val x = if [ {date +\"%Y\"} < 2023 ]; \"bash\" else \"moshell\"");
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "x",
                    ty: None,
                },
                initializer: Some(Box::new(Expr::If(If {
                    condition: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Block(Block {
                                expressions: vec![Expr::Call(Call {
                                    arguments: vec![
                                        Expr::Literal("date".into()),
                                        Expr::Literal("+\"%Y\"".into())
                                    ]
                                })]
                            })),
                            op: BinaryOperator::Less,
                            right: Box::new(Expr::Literal(Literal {
                                lexeme: "2023",
                                parsed: 2023.into()
                            }))
                        }))
                    })),
                    success_branch: Box::new(Expr::TemplateString(TemplateString {
                        parts: vec![Expr::Literal("bash".into())]
                    })),
                    fail_branch: Some(Box::new(Expr::TemplateString(TemplateString {
                        parts: vec![Expr::Literal("moshell".into())]
                    }))),
                }))),
            }),]
        )
    }

    #[test]
    fn if_else_bad_brackets() {
        let content =
            "val x = if [ $1 ] \n { echo hey; else if [ $a ]; echo hola; else echo bonjour }";
        let source = Source::unknown(content);
        let ast: ParseResult<_> = parse(source).into();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Unexpected keyword 'else'".to_string(),
                position: content.find("else").map(|p| p..p + "else".len()).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn lonely_if() {
        let content = "if [ $1 ];";
        let source = Source::unknown(content);
        let ast: ParseResult<_> = parse(source).into();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Expected statement".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }
}
