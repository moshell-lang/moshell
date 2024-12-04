use ast::control_flow::If;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType;
use lexer::token::TokenType::{Else, SemiColon};

use crate::moves::{aerated, blanks, of_type, Move};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    /// Parses a conditional expression with an optional else branch.
    pub(crate) fn parse_if(&mut self) -> ParseResult<If> {
        let start = self.cursor.force(
            of_type(TokenType::If),
            "expected 'if' at start of if expression",
        )?;
        let condition = self.statement()?;

        //skip only one semicolon if any, surrounded by newlines and spaces
        self.cursor
            .advance(aerated(of_type(SemiColon)).or(blanks()));

        //the success_branch of the if
        let success_branch = self.statement()?;

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
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        let segment = start.span.start
            ..fail_branch
                .as_ref()
                .map_or(&success_branch, |b| b.as_ref())
                .segment()
                .end;
        Ok(If {
            condition: Box::new(condition),
            success_branch: Box::new(success_branch),
            fail_branch,
            segment,
        })
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::Call;
    use ast::control_flow::If;
    use ast::group::Block;
    use ast::operation::BinaryOperator::And;
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::test::Test;
    use ast::value::{Literal, TemplateString};
    use ast::variable::{TypedVariable, VarDeclaration, VarKind, VarName, VarReference};
    use ast::Expr;
    use context::source::SourceSegmentHolder;
    use context::str_find::{find_between, find_in, rfind_between};

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::ParseResult;
    use crate::source::{identifier, literal, literal_nth};

    #[test]
    fn simple_if() {
        let source = "if [ $1 ]; echo test";
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::If(If {
                condition: Box::new(Expr::Test(Test {
                    expression: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("1".into()),
                        segment: find_in(source, "$1")
                    })),
                    segment: find_between(source, "[", "]"),
                })),
                success_branch: Box::new(Expr::Call(Call {
                    arguments: vec![literal(source, "echo"), literal(source, "test")],
                })),
                fail_branch: None,
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn if_else_if() {
        let source =
            "if echo a && [[ -f /file/exe ]]; echo test\n\n\nelse if [ $a ] \n;\n { $7 }; else $5";
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::If(If {
                condition: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Call(Call {
                        arguments: vec![literal(source, "echo"), literal(source, "a")],
                    })),
                    op: And,
                    right: Box::new(Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal(Literal {
                                parsed: "test".into(),
                                segment: find_in(source, "[[")
                            }),
                            literal(source, "-f"),
                            literal(source, "/file/exe"),
                        ],
                    }))
                })),
                success_branch: Box::new(Expr::Call(Call {
                    arguments: vec![literal_nth(source, "echo", 1), literal(source, "test")],
                })),
                fail_branch: Some(Box::new(Expr::If(If {
                    condition: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::VarReference(VarReference {
                            name: VarName::User("a".into()),
                            segment: find_in(source, "$a")
                        })),
                        segment: rfind_between(source, "[", "]"),
                    })),
                    success_branch: Box::new(Expr::Block(Block {
                        expressions: vec![Expr::VarReference(VarReference {
                            name: VarName::User("7".into()),
                            segment: find_in(source, "$7")
                        })],
                        segment: rfind_between(source, "{", "}"),
                    })),
                    fail_branch: Some(Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("5".into()),
                        segment: find_in(source, "$5")
                    }))),
                    segment: find_between(source, "if [ $a ]", "$5"),
                }))),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn if_else_if_separations() {
        let source = "if [ $1 ]; echo test; else if [ $a ]; $7 else $5";
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::If(If {
                condition: Box::new(Expr::Test(Test {
                    expression: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("1".into()),
                        segment: find_in(source, "$1")
                    })),
                    segment: find_between(source, "[", "]"),
                })),
                success_branch: Box::new(Expr::Call(Call {
                    arguments: vec![literal(source, "echo"), literal(source, "test")],
                })),
                fail_branch: Some(Box::new(Expr::If(If {
                    condition: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::VarReference(VarReference {
                            name: VarName::User("a".into()),
                            segment: find_in(source, "$a")
                        })),
                        segment: rfind_between(source, "[", "]")
                    })),
                    success_branch: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("7".into()),
                        segment: find_in(source, "$7")
                    })),
                    fail_branch: Some(Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("5".into()),
                        segment: find_in(source, "$5")
                    }))),
                    segment: find_between(source, "if [ $a ]", "$5")
                }))),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn no_separation_else() {
        let source = "if $x {} else {}";
        let ast = parse(source).expect("parse fail");
        assert_eq!(
            ast,
            vec![Expr::If(If {
                condition: Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("x".into()),
                    segment: find_in(source, "$x")
                })),
                success_branch: Box::new(Expr::Block(Block {
                    expressions: vec![],
                    segment: find_between(source, "{", "}")
                })),
                fail_branch: Some(Box::new(Expr::Block(Block {
                    expressions: vec![],
                    segment: rfind_between(source, "{", "}")
                }))),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn if_else_as_value() {
        let source = "val x = if [ {date +\"%Y\"} < 2023 ]; \"bash\" else \"moshell\"";
        let ast = parse(source).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: identifier(source, "x"),
                    ty: None,
                },
                initializer: Some(Box::new(Expr::If(If {
                    condition: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Block(Block {
                                expressions: vec![Expr::Call(Call {
                                    arguments: vec![
                                        literal(source, "date"),
                                        Expr::TemplateString(TemplateString {
                                            parts: vec![
                                                literal(source, "+"),
                                                literal(source, "%Y")
                                            ],
                                            segment: find_in(source, "+\"%Y\"")
                                        })
                                    ],
                                })],
                                segment: find_between(source, "{", "}")
                            })),
                            op: BinaryOperator::Less,
                            right: Box::new(Expr::Literal(Literal {
                                parsed: 2023.into(),
                                segment: find_in(source, "2023")
                            }))
                        })),
                        segment: find_between(source, "[", "]"),
                    })),
                    success_branch: Box::new(Expr::TemplateString(TemplateString {
                        parts: vec![literal(source, "bash")],
                        segment: find_in(source, "\"bash\"")
                    })),
                    fail_branch: Some(Box::new(Expr::TemplateString(TemplateString {
                        parts: vec![literal(source, "moshell")],
                        segment: find_in(source, "\"moshell\"")
                    }))),
                    segment: find_between(source, "if", "\"moshell\""),
                }))),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn if_else_bad_brackets() {
        let source =
            "val x = if [ $1 ] \n { echo hey; else if [ $a ]; echo hola; else echo bonjour }";
        let ast: ParseResult<_> = parse(source).into();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Unexpected keyword 'else'".to_string(),
                position: source.find("else").map(|p| p..p + "else".len()).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn lonely_if() {
        let source = "if [ $1 ];";
        let ast: ParseResult<_> = parse(source).into();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Expected statement".to_string(),
                position: source.len()..source.len(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }
}
