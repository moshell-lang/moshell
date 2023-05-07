use crate::aspects::call::CallAspect;
use crate::err::ParseErrorKind;
use crate::moves::{of_type, spaces, times, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::test::{Not, Test};
use ast::value::Literal;
use ast::Expr;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType::{SquaredLeftBracket, SquaredRightBracket};
use lexer::token::{Token, TokenType};

pub(crate) trait TestAspect<'a> {
    ///parse a not (! ..) expression.
    fn not<P>(&mut self, parse_next: P) -> ParseResult<Expr<'a>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'a>>;

    ///parse [[ ... ]] or [ .. ] expression.
    fn parse_test(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> TestAspect<'a> for Parser<'a> {
    fn not<P>(&mut self, mut parse_next: P) -> ParseResult<Expr<'a>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'a>>,
    {
        let lexeme = self
            .cursor
            .force(of_type(TokenType::Not), "expected '!'")?
            .value;
        let underlying = Box::new(parse_next(self)?);
        let segment = self.cursor.relative_pos(lexeme).start..underlying.segment().end;

        Ok(Expr::Not(Not {
            underlying,
            segment,
        }))
    }

    fn parse_test(&mut self) -> ParseResult<Expr<'a>> {
        //expect the first '[' lexeme

        let start = self.cursor.force(
            of_type(SquaredLeftBracket),
            "expected '[' at start of test expression.",
        )?;

        //if first bracket is followed by a second, then this expression is a direct call to the `test` command.
        if self.cursor.advance(of_type(SquaredLeftBracket)).is_some() {
            return self.parse_test_call(start);
        }

        if let Some(end) = self.cursor.lookahead(of_type(SquaredRightBracket)) {
            self.expected_with(
                "native test evaluation cannot be empty.",
                start.clone()..end,
                ParseErrorKind::Unexpected,
            )?;
        }

        let underlying = Box::new(self.value()?);
        let end = self.cursor.force_with(
            //expect trailing ']'
            spaces().then(of_type(SquaredRightBracket)),
            "missing ']'",
            ParseErrorKind::Unpaired(self.cursor.relative_pos(&start)),
        )?;
        Ok(Expr::Test(Test {
            expression: underlying,
            segment: self.cursor.relative_pos_ctx(start..end),
        }))
    }
}

impl<'a> Parser<'a> {
    fn parse_test_call(&mut self, start: Token) -> ParseResult<Expr<'a>> {
        let segment = self.cursor.relative_pos_ctx(start.clone());
        let call = self.call_arguments(
            Vec::new(),
            Expr::Literal(Literal {
                parsed: "test".into(),
                segment: segment.start..segment.end + 1,
            }),
            Vec::new(),
        )?;

        self.cursor.force_with(
            //expect trailing ']]'
            spaces().then(times(2, of_type(SquaredRightBracket))),
            "missing ']]'",
            ParseErrorKind::Unpaired(self.cursor.relative_pos(start)),
        )?;

        Ok(call)
    }
}

#[cfg(test)]
mod tests {
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::ParseResult;
    use crate::source::literal;
    use ast::call::Call;
    use ast::group::{Parenthesis, Subshell};
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::test::{Not, Test};
    use ast::value::{Literal, LiteralValue};
    use ast::variable::VarReference;
    use ast::Expr;
    use context::source::{Source, SourceSegmentHolder};
    use context::str_find::{find_between, find_in};
    use pretty_assertions::assert_eq;

    #[test]
    fn native_empty() {
        let source = Source::unknown("[]");
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
        let content = "[! ]";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Unexpected token ']'.".to_string(),
                position: content.len() - 1..content.len(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn call_empty() {
        let source = Source::unknown("[[]]");
        let result = parse(source).expect("parsing failed");
        assert_eq!(
            result,
            vec![Expr::Call(Call {
                path: Vec::new(),
                arguments: vec![Expr::Literal(Literal {
                    parsed: "test".into(),
                    segment: find_in(source.source, "[[")
                })],
                type_parameters: vec![],
            })]
        )
    }

    #[test]
    fn call_with_content() {
        let source = Source::unknown("[[48 -gt 100]]");
        let result = parse(source).expect("parsing failed");
        assert_eq!(
            result,
            vec![Expr::Call(Call {
                path: Vec::new(),
                arguments: vec![
                    Expr::Literal(Literal {
                        parsed: "test".into(),
                        segment: find_in(source.source, "[[")
                    }),
                    Expr::Literal(Literal {
                        parsed: LiteralValue::Int(48),
                        segment: find_in(source.source, "48"),
                    }),
                    Expr::Literal(Literal {
                        parsed: "-gt".into(),
                        segment: find_in(source.source, "-gt")
                    }),
                    Expr::Literal(Literal {
                        parsed: LiteralValue::Int(100),
                        segment: find_in(source.source, "100"),
                    }),
                ],
                type_parameters: vec![],
            })]
        )
    }

    #[test]
    fn integration() {
        let content = "echo && [ ($a == $b) ] || [[ $1 ]]";
        let source = Source::unknown(content);
        let result = parse(source).expect("parse error");
        assert_eq!(
            result,
            vec![Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Call(Call {
                        path: Vec::new(),
                        arguments: vec![literal(content, "echo")],
                        type_parameters: vec![],
                    })),
                    op: BinaryOperator::And,
                    right: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::Parenthesis(Parenthesis {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::VarReference(VarReference {
                                    name: "a",
                                    segment: find_in(content, "$a")
                                })),
                                op: BinaryOperator::EqualEqual,
                                right: Box::new(Expr::VarReference(VarReference {
                                    name: "b",
                                    segment: find_in(content, "$b")
                                })),
                            })),
                            segment: find_in(content, "($a == $b)"),
                        })),
                        segment: find_between(content, "[", "]"),
                    }))
                })),
                op: BinaryOperator::Or,
                right: Box::new(Expr::Call(Call {
                    path: Vec::new(),
                    arguments: vec![
                        Expr::Literal(Literal {
                            parsed: "test".into(),
                            segment: find_in(content, "[[")
                        }),
                        Expr::VarReference(VarReference {
                            name: "1",
                            segment: find_in(content, "$1"),
                        }),
                    ],
                    type_parameters: vec![],
                })),
            })]
        )
    }

    #[test]
    fn in_test() {
        let content = "[ test == [] ]";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Unexpected start of test expression".to_string(),
                position: content[1..].find('[').map(|p| p + 1..p + 2).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn unclosed_test() {
        let content = "[ test == $USER ";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "missing ']'".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unpaired(0..1),
            })
        )
    }

    #[test]
    fn unclosed_call() {
        let content = "[[ test == $USER ";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "missing ']]'".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unpaired(0..1),
            })
        )
    }

    #[test]
    fn not_call() {
        let source = Source::unknown("!grep -E '^[0-9]+$'");
        let result = parse(source).expect("parse fail");
        assert_eq!(
            result,
            vec![Expr::Not(Not {
                underlying: Box::new(Expr::Call(Call {
                    path: Vec::new(),
                    arguments: vec![
                        literal(source.source, "grep"),
                        literal(source.source, "-E"),
                        literal(source.source, "'^[0-9]+$'"),
                    ],
                    type_parameters: vec![],
                })),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn not() {
        let source = Source::unknown("! ($a && $b) || ! $2 == 78");
        let result = parse(source).expect("parse error");
        assert_eq!(
            result,
            vec![Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Not(Not {
                    underlying: Box::new(Expr::Subshell(Subshell {
                        expressions: vec![Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::VarReference(VarReference {
                                name: "a",
                                segment: find_in(source.source, "$a"),
                            })),
                            op: BinaryOperator::And,
                            right: Box::new(Expr::VarReference(VarReference {
                                name: "b",
                                segment: find_in(source.source, "$b"),
                            })),
                        })],
                        segment: find_in(source.source, "($a && $b)"),
                    })),
                    segment: find_in(source.source, "! ($a && $b)"),
                })),
                op: BinaryOperator::Or,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Not(Not {
                        underlying: Box::new(Expr::VarReference(VarReference {
                            name: "2",
                            segment: find_in(source.source, "$2"),
                        })),
                        segment: find_in(source.source, "! $2"),
                    })),
                    op: BinaryOperator::EqualEqual,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 78.into(),
                        segment: find_in(source.source, "78"),
                    })),
                })),
            })]
        )
    }
}
