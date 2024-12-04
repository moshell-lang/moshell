use ast::test::Test;
use ast::value::Literal;
use ast::Expr;
use lexer::token::Token;
use lexer::token::TokenType::{SquaredLeftBracket, SquaredRightBracket};

use crate::err::ParseErrorKind;
use crate::moves::{of_type, spaces, times, Move};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    /// Parses `[[ ... ]]` and `[ .. ]` expressions.
    pub(crate) fn parse_test(&mut self) -> ParseResult<Expr> {
        let start = self.cursor.force(
            of_type(SquaredLeftBracket),
            "expected '[' at start of test expression.",
        )?;

        //if first bracket is followed by a second, then this expression is a direct call to the `test` command.
        if self.cursor.advance(of_type(SquaredLeftBracket)).is_some() {
            return self.parse_test_call(start);
        }

        if let Some(end) = self.cursor.advance(of_type(SquaredRightBracket)) {
            self.expected_with(
                "native test evaluation cannot be empty.",
                start.span.start..end.span.end,
                ParseErrorKind::Unexpected,
            )?;
        }

        let underlying = Box::new(self.value().inspect_err(|_| {
            self.repos_to_top_delimiter();
        })?);
        let end = self.cursor.force_with(
            //expect trailing ']'
            spaces().then(of_type(SquaredRightBracket)),
            "missing ']'",
            ParseErrorKind::Unpaired(start.span.clone()),
        )?;
        Ok(Expr::Test(Test {
            expression: underlying,
            segment: start.span.start..end.span.end,
        }))
    }

    fn parse_test_call(&mut self, start: Token) -> ParseResult<Expr> {
        let segment = start.span;
        let call = self.call_arguments(Expr::Literal(Literal {
            parsed: "test".into(),
            segment: segment.start..segment.end + 1,
        }))?;

        self.cursor.force_with(
            //expect trailing ']]'
            spaces().then(times(2, of_type(SquaredRightBracket))),
            "missing ']]'",
            ParseErrorKind::Unpaired(segment),
        )?;

        Ok(call)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::{Call, ProgrammaticCall};
    use ast::group::{Parenthesis, Subshell};
    use ast::operation::{BinaryOperation, BinaryOperator, UnaryOperation, UnaryOperator};
    use ast::r#use::InclusionPathItem;
    use ast::test::Test;
    use ast::value::{Literal, LiteralValue};
    use ast::variable::{VarName, VarReference};
    use ast::Expr;
    use context::source::SourceSegmentHolder;
    use context::str_find::{find_between, find_in};

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::ParseResult;
    use crate::source::{identifier, literal};

    #[test]
    fn native_empty() {
        let source = "[]";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "native test evaluation cannot be empty.".to_string(),
                position: source.segment(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn native_empty_not() {
        let source = "[! ]";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Unexpected token ']'.".to_string(),
                position: source.len() - 1..source.len(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn call_empty() {
        let source = "[[]]";
        let result = parse(source).expect("parsing failed");
        assert_eq!(
            result,
            vec![Expr::Call(Call {
                arguments: vec![Expr::Literal(Literal {
                    parsed: "test".into(),
                    segment: find_in(source, "[[")
                })],
            })]
        )
    }

    #[test]
    fn call_with_source() {
        let source = "[[48 -gt 100]]";
        let result = parse(source).expect("parsing failed");
        assert_eq!(
            result,
            vec![Expr::Call(Call {
                arguments: vec![
                    Expr::Literal(Literal {
                        parsed: "test".into(),
                        segment: find_in(source, "[[")
                    }),
                    Expr::Literal(Literal {
                        parsed: LiteralValue::Int(48),
                        segment: find_in(source, "48"),
                    }),
                    Expr::Literal(Literal {
                        parsed: "-gt".into(),
                        segment: find_in(source, "-gt")
                    }),
                    Expr::Literal(Literal {
                        parsed: LiteralValue::Int(100),
                        segment: find_in(source, "100"),
                    }),
                ],
            })]
        )
    }

    #[test]
    fn integration() {
        let source = "echo && [ ($a == $b) ] || [[ $1 ]]";
        let result = parse(source).expect("parse error");
        assert_eq!(
            result,
            vec![Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Call(Call {
                        arguments: vec![literal(source, "echo")],
                    })),
                    op: BinaryOperator::And,
                    right: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::Parenthesis(Parenthesis {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::VarReference(VarReference {
                                    name: VarName::User("a".into()),
                                    segment: find_in(source, "$a")
                                })),
                                op: BinaryOperator::EqualEqual,
                                right: Box::new(Expr::VarReference(VarReference {
                                    name: VarName::User("b".into()),
                                    segment: find_in(source, "$b")
                                })),
                            })),
                            segment: find_in(source, "($a == $b)"),
                        })),
                        segment: find_between(source, "[", "]"),
                    }))
                })),
                op: BinaryOperator::Or,
                right: Box::new(Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            parsed: "test".into(),
                            segment: find_in(source, "[[")
                        }),
                        Expr::VarReference(VarReference {
                            name: VarName::User("1".into()),
                            segment: find_in(source, "$1"),
                        }),
                    ],
                })),
            })]
        )
    }

    #[test]
    fn in_test() {
        let source = "[ 'test' == [] ]";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Unexpected start of test expression".to_string(),
                position: source[1..].find('[').map(|p| p + 1..p + 2).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn unclosed_test() {
        let source = "[ 'test' == $USER ";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "missing ']'".to_string(),
                position: source.len()..source.len(),
                kind: ParseErrorKind::Unpaired(0..1),
            })
        )
    }

    #[test]
    fn unclosed_call() {
        let source = "[[ test == $USER ";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "missing ']]'".to_string(),
                position: source.len()..source.len(),
                kind: ParseErrorKind::Unpaired(0..1),
            })
        )
    }

    #[test]
    fn not_call() {
        let source = "!grep -E '^[0-9]+$'";
        let result = parse(source).expect("parse fail");
        assert_eq!(
            result,
            vec![Expr::Unary(UnaryOperation {
                op: UnaryOperator::Not,
                expr: Box::new(Expr::Call(Call {
                    arguments: vec![
                        literal(source, "grep"),
                        literal(source, "-E"),
                        literal(source, "'^[0-9]+$'"),
                    ],
                })),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn not() {
        let source = "! ($a && $b) || ! $2 == 78";
        let result = parse(source).expect("parse error");
        assert_eq!(
            result,
            vec![Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Unary(UnaryOperation {
                    op: UnaryOperator::Not,
                    expr: Box::new(Expr::Subshell(Subshell {
                        expressions: vec![Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::VarReference(VarReference {
                                name: VarName::User("a".into()),
                                segment: find_in(source, "$a"),
                            })),
                            op: BinaryOperator::And,
                            right: Box::new(Expr::VarReference(VarReference {
                                name: VarName::User("b".into()),
                                segment: find_in(source, "$b"),
                            })),
                        })],
                        segment: find_in(source, "($a && $b)"),
                    })),
                    segment: find_in(source, "! ($a && $b)"),
                })),
                op: BinaryOperator::Or,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Unary(UnaryOperation {
                        op: UnaryOperator::Not,
                        expr: Box::new(Expr::VarReference(VarReference {
                            name: VarName::User("2".into()),
                            segment: find_in(source, "$2"),
                        })),
                        segment: find_in(source, "! $2"),
                    })),
                    op: BinaryOperator::EqualEqual,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 78.into(),
                        segment: find_in(source, "78"),
                    })),
                })),
            })]
        )
    }

    #[test]
    fn not_exe() {
        let source = "! ./test.sh || ! foo && !bar() + 1";
        let result = parse(source).expect("parse error");
        assert_eq!(
            result,
            vec![Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Unary(UnaryOperation {
                    op: UnaryOperator::Not,
                    expr: Box::new(Expr::Call(Call {
                        arguments: vec![literal(source, "./test.sh")],
                    })),
                    segment: find_in(source, "! ./test.sh"),
                })),
                op: BinaryOperator::Or,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Unary(UnaryOperation {
                        op: UnaryOperator::Not,
                        expr: Box::new(Expr::Call(Call {
                            arguments: vec![literal(source, "foo")],
                        })),
                        segment: find_in(source, "! foo"),
                    })),
                    op: BinaryOperator::And,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Unary(UnaryOperation {
                            op: UnaryOperator::Not,
                            expr: Box::new(Expr::ProgrammaticCall(ProgrammaticCall {
                                path: vec![InclusionPathItem::Symbol(identifier(source, "bar"))],
                                arguments: vec![],
                                type_parameters: vec![],
                                segment: find_in(source, "bar()"),
                            })),
                            segment: find_in(source, "!bar()"),
                        })),
                        op: BinaryOperator::Plus,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in(source, "1"),
                        })),
                    })),
                })),
            })]
        )
    }
}
