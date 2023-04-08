use crate::moves::{aerated, blanks, of_type, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::control_flow::If;
use ast::Expr;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType;
use lexer::token::TokenType::{Else, SemiColon};

///parser aspect for if and else expressions.
pub trait IfElseAspect<'a> {
    ///parse a if with optional else expression.
    /// `parse_branch` argument defines how the branches must be parsed.
    fn parse_if<F>(&mut self, parse_branch: F) -> ParseResult<If<'a>>
    where
        F: FnMut(&mut Self) -> ParseResult<Expr<'a>>;
}

impl<'a> IfElseAspect<'a> for Parser<'a> {
    fn parse_if<F>(&mut self, mut parse_branch: F) -> ParseResult<If<'a>>
    where
        F: FnMut(&mut Self) -> ParseResult<Expr<'a>>,
    {
        let start = self.cursor.force(
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

        let segment = self.cursor.relative_pos(start.value).start
            ..fail_branch
                .as_ref()
                .map(|b| b.as_ref())
                .unwrap_or(&success_branch)
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
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::ParseResult;
    use crate::source::{find_between, find_in, literal, literal_nth, rfind_between};
    use ast::call::Call;
    use ast::control_flow::If;
    use ast::group::Block;
    use ast::operation::BinaryOperator::And;
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::test::Test;
    use ast::value::{Literal, TemplateString};
    use ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
    use ast::Expr;
    use context::source::{Source, SourceSegmentHolder};
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_if() {
        let content = "if [ $1 ]; echo test";
        let source = Source::unknown(content);
        let ast = parse(source.clone()).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::If(If {
                condition: Box::new(Expr::Test(Test {
                    expression: Box::new(Expr::VarReference(VarReference {
                        name: "1",
                        segment: find_in(content, "$1")
                    })),
                    segment: find_between(content, "[", "]"),
                })),
                success_branch: Box::new(Expr::Call(Call {
                    arguments: vec![
                        literal(source.source, "echo"),
                        literal(source.source, "test")
                    ],
                    type_parameters: vec![],
                })),
                fail_branch: None,
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn if_else_if() {
        let content =
            "if echo a && [[ -f /file/exe ]]; echo test\n\n\nelse if [ $a ] \n;\n { $7 }; else $5";
        let source = Source::unknown(content.clone());
        let ast = parse(source.clone()).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::If(If {
                condition: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Call(Call {
                        arguments: vec![literal(content, "echo"), literal(content, "a")],
                        type_parameters: vec![],
                    })),
                    op: And,
                    right: Box::new(Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal(Literal {
                                parsed: "test".into(),
                                segment: find_in(content, "[[")
                            }),
                            literal(content, "-f"),
                            literal(content, "/file/exe"),
                        ],
                        type_parameters: vec![],
                    }))
                })),
                success_branch: Box::new(Expr::Call(Call {
                    arguments: vec![literal_nth(content, "echo", 1), literal(content, "test")],
                    type_parameters: vec![],
                })),
                fail_branch: Some(Box::new(Expr::If(If {
                    condition: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::VarReference(VarReference {
                            name: "a",
                            segment: find_in(content, "$a")
                        })),
                        segment: rfind_between(content, "[", "]"),
                    })),
                    success_branch: Box::new(Expr::Block(Block {
                        expressions: vec![Expr::VarReference(VarReference {
                            name: "7",
                            segment: find_in(content, "$7")
                        })],
                        segment: rfind_between(content, "{", "}"),
                    })),
                    fail_branch: Some(Box::new(Expr::VarReference(VarReference {
                        name: "5",
                        segment: find_in(content, "$5")
                    }))),
                    segment: find_between(content, "if [ $a ]", "$5"),
                }))),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn if_else_if_separations() {
        let content = "if [ $1 ]; echo test; else if [ $a ]; $7 else $5";
        let source = Source::unknown(content);
        let ast = parse(source.clone()).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::If(If {
                condition: Box::new(Expr::Test(Test {
                    expression: Box::new(Expr::VarReference(VarReference {
                        name: "1",
                        segment: find_in(content, "$1")
                    })),
                    segment: find_between(content, "[", "]"),
                })),
                success_branch: Box::new(Expr::Call(Call {
                    arguments: vec![literal(content, "echo"), literal(content, "test")],
                    type_parameters: vec![],
                })),
                fail_branch: Some(Box::new(Expr::If(If {
                    condition: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::VarReference(VarReference {
                            name: "a",
                            segment: find_in(content, "$a")
                        })),
                        segment: rfind_between(content, "[", "]")
                    })),
                    success_branch: Box::new(Expr::VarReference(VarReference {
                        name: "7",
                        segment: find_in(content, "$7")
                    })),
                    fail_branch: Some(Box::new(Expr::VarReference(VarReference {
                        name: "5",
                        segment: find_in(content, "$5")
                    }))),
                    segment: find_between(content, "if [ $a ]", "$5")
                }))),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn no_separation_else() {
        let source = Source::unknown("if $x {} else {}");
        let ast = parse(source.clone()).expect("parse fail");
        assert_eq!(
            ast,
            vec![Expr::If(If {
                condition: Box::new(Expr::VarReference(VarReference {
                    name: "x",
                    segment: find_in(source.source, "$x")
                })),
                success_branch: Box::new(Expr::Block(Block {
                    expressions: vec![],
                    segment: find_between(source.source, "{", "}")
                })),
                fail_branch: Some(Box::new(Expr::Block(Block {
                    expressions: vec![],
                    segment: rfind_between(source.source, "{", "}")
                }))),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn if_else_as_value() {
        let content = "val x = if [ {date +\"%Y\"} < 2023 ]; \"bash\" else \"moshell\"";
        let source = Source::unknown(content);
        let ast = parse(source.clone()).expect("parse failed");
        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "x",
                    ty: None,
                    segment: find_in(content, "x")
                },
                initializer: Some(Box::new(Expr::If(If {
                    condition: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Block(Block {
                                expressions: vec![Expr::Call(Call {
                                    arguments: vec![
                                        literal(content, "date"),
                                        Expr::TemplateString(TemplateString {
                                            parts: vec![
                                                literal(content, "+"),
                                                literal(content, "\"%Y\"")
                                            ],
                                        })
                                    ],
                                    type_parameters: vec![],
                                })],
                                segment: find_between(content, "{", "}")
                            })),
                            op: BinaryOperator::Less,
                            right: Box::new(Expr::Literal(Literal {
                                parsed: 2023.into(),
                                segment: find_in(content, "2023")
                            }))
                        })),
                        segment: find_between(content, "[", "]"),
                    })),
                    success_branch: Box::new(Expr::TemplateString(TemplateString {
                        parts: vec![literal(content, "\"bash\"")],
                    })),
                    fail_branch: Some(Box::new(Expr::TemplateString(TemplateString {
                        parts: vec![literal(content, "\"moshell\"")],
                    }))),
                    segment: find_between(content, "if", "\"moshell\""),
                }))),
                segment: source.segment()
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
