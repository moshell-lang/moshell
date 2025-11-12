use crate::parser::Parser;
use lexer::token::TokenType;

const NOT_AN_OPERATOR: u8 = 0;

/// Gets the binding power of an infix operator for a Pratt parser.
///
/// If the token is not an infix operator, 0 is returned.
pub(crate) const fn infix_precedence(tok: TokenType) -> u8 {
    use TokenType::*;
    match tok {
        Equal => 1,
        DotDot => 2,
        Or => 3,
        And => 4,
        EqualEqual | NotEqual | Less | LessEqual | Greater | GreaterEqual => 5,
        Plus | Minus => 6,
        Star | Slash | Percent => 7,
        As => 8,
        _ => NOT_AN_OPERATOR,
    }
}

/// Gets the binding power of the current token in a shell context.
///
/// If the token is not an infix operator, 0 is returned.
pub(crate) fn shell_infix_precedence(parser: &Parser) -> u8 {
    use TokenType::*;
    match parser.cursor.peek().token_type {
        Or => 1,
        And => 2,
        Bar => 3,
        _ if parser.is_at_redirection_sign() => 4,
        _ => NOT_AN_OPERATOR,
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::Call;
    use ast::group::{Parenthesis, Subshell};
    use ast::operation::BinaryOperation;
    use ast::operation::BinaryOperator::*;
    use ast::value::Literal;
    use ast::Expr;
    use context::source::SourceSegmentHolder;
    use context::str_find::{find_between, find_in, find_in_nth};

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parser::Parser;
    use crate::source::{literal, literal_nth};

    #[test]
    fn is_left_associative() {
        let source = "9 - 4 + 11";
        let mut parser = Parser::new(source);
        let ast = parser.value().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: 9.into(),
                        segment: find_in(source, "9")
                    })),
                    op: Minus,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 4.into(),
                        segment: find_in(source, "4")
                    })),
                })),
                op: Plus,
                right: Box::new(Expr::Literal(Literal {
                    parsed: 11.into(),
                    segment: find_in(source, "11")
                })),
            })
        )
    }

    #[test]
    fn has_priority() {
        let source = "1 && 2 || \\\n 3 || 4 && 5";
        let mut parser = Parser::new(source);
        let ast = parser.value().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in(source, "1")
                        })),
                        op: And,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 2.into(),
                            segment: find_in(source, "2")
                        })),
                    })),
                    op: Or,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 3.into(),
                        segment: find_in(source, "3")
                    })),
                })),
                op: Or,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: 4.into(),
                        segment: find_in(source, "4")
                    })),
                    op: And,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 5.into(),
                        segment: find_in(source, "5")
                    })),
                })),
            })
        )
    }

    #[test]
    fn explicit_priority() {
        let source = "1 \\\n+\\\n (2 + 3)";
        let mut parser = Parser::new(source);
        let ast = parser.value().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Literal(Literal {
                    parsed: 1.into(),
                    segment: find_in(source, "1")
                })),
                op: Plus,
                right: Box::new(Expr::Parenthesis(Parenthesis {
                    expression: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 2.into(),
                            segment: find_in(source, "2")
                        })),
                        op: Plus,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 3.into(),
                            segment: find_in(source, "3")
                        })),
                    })),
                    segment: find_between(source, "(", ")")
                })),
            })
        )
    }

    #[test]
    fn arithmetic_priority() {
        let source = "1 + 2 * 3";
        let mut parser = Parser::new(source);
        let ast = parser.value().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Literal(Literal {
                    parsed: 1.into(),
                    segment: find_in(source, "1")
                })),
                op: Plus,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: 2.into(),
                        segment: find_in(source, "2")
                    })),
                    op: Times,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 3.into(),
                        segment: find_in(source, "3")
                    })),
                })),
            })
        )
    }

    #[test]
    fn complete_prioritization_test() {
        let source = "1 +\\\n 2 \\\n*\\\n 3\\\n < 874\\\n / 78 \\\n||\\\n 7\\\n % 4 \\\n== 3 \\\n&& \\\n7 ==\\\n 1";
        let mut parser = Parser::new(source);
        let ast = parser.value().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in(source, "1")
                        })),
                        op: Plus,
                        right: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                parsed: 2.into(),
                                segment: find_in(source, "2")
                            })),
                            op: Times,
                            right: Box::new(Expr::Literal(Literal {
                                parsed: 3.into(),
                                segment: find_in(source, "3")
                            })),
                        })),
                    })),
                    op: Less,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 874.into(),
                            segment: find_in(source, "874")
                        })),
                        op: Divide,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 78.into(),
                            segment: find_in(source, "78")
                        })),
                    })),
                })),
                op: Or,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                parsed: 7.into(),
                                segment: find_in_nth(source, "7", 2)
                            })),
                            op: Modulo,
                            right: Box::new(Expr::Literal(Literal {
                                parsed: 4.into(),
                                segment: find_in_nth(source, "4", 1)
                            })),
                        })),
                        op: EqualEqual,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 3.into(),
                            segment: find_in_nth(source, "3", 1)
                        })),
                    })),
                    op: And,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 7.into(),
                            segment: find_in_nth(source, "7", 3)
                        })),
                        op: EqualEqual,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in_nth(source, "1", 1)
                        })),
                    })),
                })),
            }),
        );
    }

    #[test]
    fn unterminated_expr() {
        let source = "(1 + 2 * )";
        let mut parser = Parser::new(source);
        let result = parser.value();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Unexpected token ')'.".to_string(),
                position: source.find(')').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn bin_expression_in_group() {
        let source = "(1 + 2 * 3)";
        let mut parser = Parser::new(source);
        let ast = parser.value().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Parenthesis(Parenthesis {
                expression: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: 1.into(),
                        segment: find_in(source, "1")
                    })),
                    op: Plus,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 2.into(),
                            segment: find_in(source, "2")
                        })),
                        op: Times,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 3.into(),
                            segment: find_in(source, "3")
                        })),
                    })),
                })),
                segment: source.segment(),
            })
        )
    }

    #[test]
    fn exitcode_operators() {
        let source = "(echo hello && echo world ) || echo damn";
        let mut parser = Parser::new(source);
        let ast = parser.statement().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Subshell(Subshell {
                    expressions: vec![Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Call(Call {
                            arguments: vec![literal(source, "echo"), literal(source, "hello"),],
                        })),
                        op: And,
                        right: Box::new(Expr::Call(Call {
                            arguments: vec![
                                literal_nth(source, "echo", 1),
                                literal(source, "world"),
                            ],
                        })),
                    })],
                    segment: find_between(source, "(", ")")
                })),
                op: Or,
                right: Box::new(Expr::Call(Call {
                    arguments: vec![literal_nth(source, "echo", 2), literal(source, "damn"),],
                })),
            })
        )
    }

    #[test]
    fn escaped_operators() {
        let source = "(echo hello \\& world \\);) || echo damn";
        let mut parser = Parser::new(source);
        let ast = parser.statement().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Subshell(Subshell {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![
                            literal(source, "echo"),
                            literal(source, "hello"),
                            literal(source, "&"),
                            literal(source, "world"),
                            literal(source, ")"),
                        ],
                    })],
                    segment: find_between(source, "(", ";)")
                })),
                op: Or,
                right: Box::new(Expr::Call(Call {
                    arguments: vec![literal_nth(source, "echo", 1), literal(source, "damn"),],
                })),
            })
        )
    }
}
