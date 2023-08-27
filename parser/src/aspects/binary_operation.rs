use ast::operation::{BinaryOperation, BinaryOperator};
use ast::Expr;
use lexer::token::TokenType;

use crate::moves::{bin_op, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};

/// Gets the binding power of an infix operator for a Pratt parser.
///
/// If the token is not an infix operator, 0 is returned.
pub const fn infix_precedence(tok: TokenType) -> u8 {
    const NOT_AN_OPERATOR: u8 = 0;
    use TokenType::*;
    match tok {
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

/// A parser aspect to parse any kind of binary operations.
pub trait BinaryOperationsAspect<'p> {
    /// Parses a binary operation expression.
    ///
    /// `expr`: the left-hand side of the binary operation to start with
    /// `parse_next`: a function that parses the next expression to the right of the binary operation
    fn binary_operation<P>(&mut self, expr: Expr<'p>, parse_next: P) -> ParseResult<Expr<'p>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'p>> + Copy;
}

impl<'p> BinaryOperationsAspect<'p> for Parser<'p> {
    fn binary_operation<P>(&mut self, expr: Expr<'p>, parse: P) -> ParseResult<Expr<'p>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'p>> + Copy,
    {
        // Parse a top-level tree with the lowest precedence
        self.binary_operation_internal(expr, parse, u8::MIN)
    }
}

impl<'p> Parser<'p> {
    fn binary_operation_internal<P>(
        &mut self,
        mut expr: Expr<'p>,
        mut parse: P,
        min_precedence: u8,
    ) -> ParseResult<Expr<'p>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'p>> + Copy,
    {
        while let Some(binary_op) = self.cursor.lookahead(spaces().then(bin_op())) {
            let op =
                BinaryOperator::try_from(binary_op.token_type).expect("Invalid binary operator");
            let op_priority = infix_precedence(binary_op.token_type);
            if op_priority < min_precedence {
                return Ok(expr);
            }
            self.cursor.advance(spaces().then(bin_op()));

            let mut right = parse(self)?;
            while let Some(binary_op) = self.cursor.lookahead(spaces().then(bin_op())) {
                let local_op_priority = infix_precedence(binary_op.token_type);
                if local_op_priority <= op_priority {
                    break;
                }
                right = self.binary_operation_internal(right, parse, local_op_priority)?;
            }
            expr = Expr::Binary(BinaryOperation {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            });
        }
        Ok(expr)
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
    use context::source::{Source, SourceSegmentHolder};
    use context::str_find::{find_between, find_in, find_in_nth};

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parser::Parser;
    use crate::source::{literal, literal_nth};

    #[test]
    fn is_left_associative() {
        let source = Source::unknown("9 - 4 + 11");
        let mut parser = Parser::new(source);
        let ast = parser.value().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: 9.into(),
                        segment: find_in(source.source, "9")
                    })),
                    op: Minus,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 4.into(),
                        segment: find_in(source.source, "4")
                    })),
                })),
                op: Plus,
                right: Box::new(Expr::Literal(Literal {
                    parsed: 11.into(),
                    segment: find_in(source.source, "11")
                })),
            })
        )
    }

    #[test]
    fn has_priority() {
        let source = Source::unknown("1 && 2 || \\\n 3 || 4 && 5");
        let mut parser = Parser::new(source);
        let ast = parser.value().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in(source.source, "1")
                        })),
                        op: And,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 2.into(),
                            segment: find_in(source.source, "2")
                        })),
                    })),
                    op: Or,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 3.into(),
                        segment: find_in(source.source, "3")
                    })),
                })),
                op: Or,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: 4.into(),
                        segment: find_in(source.source, "4")
                    })),
                    op: And,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 5.into(),
                        segment: find_in(source.source, "5")
                    })),
                })),
            })
        )
    }

    #[test]
    fn explicit_priority() {
        let source = Source::unknown("1 \\\n+\\\n (2 + 3)");
        let mut parser = Parser::new(source);
        let ast = parser.value().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Literal(Literal {
                    parsed: 1.into(),
                    segment: find_in(source.source, "1")
                })),
                op: Plus,
                right: Box::new(Expr::Parenthesis(Parenthesis {
                    expression: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 2.into(),
                            segment: find_in(source.source, "2")
                        })),
                        op: Plus,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 3.into(),
                            segment: find_in(source.source, "3")
                        })),
                    })),
                    segment: find_between(source.source, "(", ")")
                })),
            })
        )
    }

    #[test]
    fn arithmetic_priority() {
        let source = Source::unknown("1 + 2 * 3");
        let mut parser = Parser::new(source);
        let ast = parser.value().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Literal(Literal {
                    parsed: 1.into(),
                    segment: find_in(source.source, "1")
                })),
                op: Plus,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: 2.into(),
                        segment: find_in(source.source, "2")
                    })),
                    op: Times,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 3.into(),
                        segment: find_in(source.source, "3")
                    })),
                })),
            })
        )
    }

    #[test]
    fn complete_prioritization_test() {
        let source = Source::unknown("1 +\\\n 2 \\\n*\\\n 3\\\n < 874\\\n / 78 \\\n||\\\n 7\\\n % 4 \\\n== 3 \\\n&& \\\n7 ==\\\n 1");
        let mut parser = Parser::new(source);
        let ast = parser.value().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in(source.source, "1")
                        })),
                        op: Plus,
                        right: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                parsed: 2.into(),
                                segment: find_in(source.source, "2")
                            })),
                            op: Times,
                            right: Box::new(Expr::Literal(Literal {
                                parsed: 3.into(),
                                segment: find_in(source.source, "3")
                            })),
                        })),
                    })),
                    op: Less,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 874.into(),
                            segment: find_in(source.source, "874")
                        })),
                        op: Divide,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 78.into(),
                            segment: find_in(source.source, "78")
                        })),
                    })),
                })),
                op: Or,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                parsed: 7.into(),
                                segment: find_in_nth(source.source, "7", 2)
                            })),
                            op: Modulo,
                            right: Box::new(Expr::Literal(Literal {
                                parsed: 4.into(),
                                segment: find_in_nth(source.source, "4", 1)
                            })),
                        })),
                        op: EqualEqual,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 3.into(),
                            segment: find_in_nth(source.source, "3", 1)
                        })),
                    })),
                    op: And,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 7.into(),
                            segment: find_in_nth(source.source, "7", 3)
                        })),
                        op: EqualEqual,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in_nth(source.source, "1", 1)
                        })),
                    })),
                })),
            }),
        );
    }

    #[test]
    fn unterminated_expr() {
        let content = "(1 + 2 * )";
        let source = Source::unknown(content);
        let mut parser = Parser::new(source);
        let result = parser.value();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Unexpected token ')'.".to_string(),
                position: content.find(')').map(|p| (p..p + 1)).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn bin_expression_in_group() {
        let source = Source::unknown("(1 + 2 * 3)");
        let mut parser = Parser::new(source);
        let ast = parser.value().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Parenthesis(Parenthesis {
                expression: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: 1.into(),
                        segment: find_in(source.source, "1")
                    })),
                    op: Plus,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            parsed: 2.into(),
                            segment: find_in(source.source, "2")
                        })),
                        op: Times,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 3.into(),
                            segment: find_in(source.source, "3")
                        })),
                    })),
                })),
                segment: source.segment(),
            })
        )
    }

    #[test]
    fn exitcode_operators() {
        let source = Source::unknown("(echo hello && echo world ) || echo damn");
        let mut parser = Parser::new(source);
        let ast = parser.statement().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Subshell(Subshell {
                    expressions: vec![Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Call(Call {
                            arguments: vec![
                                literal(source.source, "echo"),
                                literal(source.source, "hello"),
                            ],
                        })),
                        op: And,
                        right: Box::new(Expr::Call(Call {
                            arguments: vec![
                                literal_nth(source.source, "echo", 1),
                                literal(source.source, "world"),
                            ],
                        })),
                    })],
                    segment: find_between(source.source, "(", ")")
                })),
                op: Or,
                right: Box::new(Expr::Call(Call {
                    arguments: vec![
                        literal_nth(source.source, "echo", 2),
                        literal(source.source, "damn"),
                    ],
                })),
            })
        )
    }

    #[test]
    fn escaped_operators() {
        let source = Source::unknown("(echo hello \\& world \\);) || echo damn");
        let mut parser = Parser::new(source);
        let ast = parser.statement().expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Subshell(Subshell {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![
                            literal(source.source, "echo"),
                            literal(source.source, "hello"),
                            literal(source.source, "&"),
                            literal(source.source, "world"),
                            literal(source.source, ")"),
                        ],
                    })],
                    segment: find_between(source.source, "(", ";)")
                })),
                op: Or,
                right: Box::new(Expr::Call(Call {
                    arguments: vec![
                        literal_nth(source.source, "echo", 1),
                        literal(source.source, "damn"),
                    ],
                })),
            })
        )
    }
}
