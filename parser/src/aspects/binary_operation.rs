use crate::moves::{bin_op, eox, spaces, word_seps, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::operation::{BinaryOperation, BinaryOperator};
use ast::Expr;

/// a parser aspect to parse any kind of binary operations
pub trait BinaryOperationsAspect<'p> {
    ///Parses a binary operation expression
    /// `eox`: a selector to define where the binary operation expression hits it end.
    fn binary_operation<P>(&mut self, parse_next: P) -> ParseResult<Expr<'p>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'p>>;

    ///Parses the operator and right arm of a left-defined binary operation.
    /// `left`: the left arm of the binary operator.
    /// `eox`: a selector to define where the binary operation expression hits it end.
    fn binary_operation_right<P>(&mut self, left: Expr<'p>, parse_next: P) -> ParseResult<Expr<'p>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'p>>;
}

impl<'p> BinaryOperationsAspect<'p> for Parser<'p> {
    fn binary_operation<P>(&mut self, mut parse: P) -> ParseResult<Expr<'p>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'p>>,
    {
        let left = parse(self)?;
        self.binary_operation_right(left, parse)
    }

    fn binary_operation_right<P>(&mut self, left: Expr<'p>, mut parse: P) -> ParseResult<Expr<'p>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'p>>,
    {
        //parsing a top-level tree operation with fewest priority
        self.binary_op_right_internal(i8::MIN, left, &mut parse)
    }
}

impl<'p> Parser<'p> {
    //Parses a binary operation tree as long as it does not hits an operation with smaller priority.
    fn binary_op_right_internal<P>(
        &mut self,
        priority: i8,
        left: Expr<'p>,
        parse: &mut P,
    ) -> ParseResult<Expr<'p>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'p>>,
    {
        let mut operation = self.binary_operation_internal(left, parse)?;
        macro_rules! hit_eox {
            () => {
                self.cursor.lookahead(spaces().then(eox())).is_some()
            };
        }

        while !hit_eox!() && self.has_priority(priority) {
            operation = self.binary_operation_internal(operation, parse)?;
        }

        Ok(operation)
    }

    //does current operator priority has priority over next binary operator ?
    fn has_priority(&self, current_priority: i8) -> bool {
        self.cursor
            .lookahead(word_seps().then(bin_op()))
            .map(|t| {
                BinaryOperator::try_from(t.token_type)
                    .expect("conception error") //cannot fail
                    .priority()
                    .saturating_sub(current_priority)
            })
            .map(|comp| comp > 0)
            .unwrap_or(false)
    }

    fn binary_operation_internal<P>(
        &mut self,
        left: Expr<'p>,
        parse: &mut P,
    ) -> ParseResult<Expr<'p>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'p>>,
    {
        //current expressions' infix operator
        let operator = self.cursor.advance(word_seps().then(bin_op())).map(|t| {
            BinaryOperator::try_from(t.token_type) //cannot fail
                .expect("conception error")
        });

        if operator.is_none() {
            return Ok(left);
        }

        let operator = operator.unwrap();

        let operator_priority = operator.priority();

        //parse next expression
        let right = parse(self)?;

        //comparison between current operator's priority and next operator (if any)
        //is 0 if priorities are same or if there is no next operator.
        //is < 0 if current operator's priority is higher
        //is > 0 if current operator's priority is smaller
        let priority_comparison = self
            .cursor
            .lookahead(word_seps().then(bin_op()))
            .map(|t| {
                operator_priority
                    - BinaryOperator::try_from(t.token_type)
                        .expect("not a valid operator")
                        .priority()
            })
            .unwrap_or(0);

        let result = if priority_comparison > 0 {
            //current binary operator has most priority so we directly return it
            Expr::Binary(BinaryOperation {
                left: Box::new(left),
                op: operator,
                right: Box::new(right),
            })
        } else if priority_comparison == 0 {
            //same priority so we can continue to parse to the right,
            // passing current binary operation as the left operation.
            self.binary_op_right_internal(
                operator_priority,
                Expr::Binary(BinaryOperation {
                    left: Box::new(left),
                    op: operator,
                    right: Box::new(right),
                }),
                parse,
            )?
        } else {
            //priority is fewer, so we let the priority to the right by continuing to parse
            Expr::Binary(BinaryOperation {
                left: Box::new(left),
                op: operator,
                right: Box::new(self.binary_op_right_internal(operator_priority, right, parse)?),
            })
        };

        return Ok(result);
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use context::source::Source;

    use crate::aspects::binary_operation::BinaryOperationsAspect;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parser::Parser;
    use ast::callable::Call;
    use ast::group::{Parenthesis, Subshell};
    use ast::operation::BinaryOperation;
    use ast::operation::BinaryOperator::*;
    use ast::value::Literal;
    use ast::Expr;

    #[test]
    fn is_left_associative() {
        let source = Source::unknown("1 && 2 || \\\n 3 || 4 && 5");
        let mut parser = Parser::new(source);
        let ast = parser
            .binary_operation(Parser::next_statement)
            .expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                lexeme: "1",
                                parsed: 1.into(),
                            })),
                            op: And,
                            right: Box::new(Expr::Literal(Literal {
                                lexeme: "2",
                                parsed: 2.into(),
                            })),
                        })),
                        op: Or,
                        right: Box::new(Expr::Literal(Literal {
                            lexeme: "3",
                            parsed: 3.into(),
                        })),
                    })),
                    op: Or,
                    right: Box::new(Expr::Literal(Literal {
                        lexeme: "4",
                        parsed: 4.into(),
                    })),
                })),
                op: And,
                right: Box::new(Expr::Literal(Literal {
                    lexeme: "5",
                    parsed: 5.into(),
                })),
            }),
        )
    }

    #[test]
    fn explicit_priority() {
        let source = Source::unknown("1 \\\n+\\\n (2 + 3)");
        let mut parser = Parser::new(source);
        let ast = parser
            .binary_operation(Parser::next_value)
            .expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Literal(Literal {
                    lexeme: "1",
                    parsed: 1.into(),
                })),
                op: Plus,
                right: Box::new(Expr::Parenthesis(Parenthesis {
                    expression: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            lexeme: "2",
                            parsed: 2.into(),
                        })),
                        op: Plus,
                        right: Box::new(Expr::Literal(Literal {
                            lexeme: "3",
                            parsed: 3.into(),
                        })),
                    })),
                })),
            })
        )
    }

    #[test]
    fn arithmetic_priority() {
        let source = Source::unknown("1 + 2 * 3");
        let mut parser = Parser::new(source);
        let ast = parser
            .binary_operation(Parser::next_value)
            .expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Literal(Literal {
                    lexeme: "1",
                    parsed: 1.into(),
                })),
                op: Plus,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        lexeme: "2",
                        parsed: 2.into(),
                    })),
                    op: Times,
                    right: Box::new(Expr::Literal(Literal {
                        lexeme: "3",
                        parsed: 3.into(),
                    })),
                })),
            })
        )
    }

    #[test]
    fn complete_prioritization_test() {
        let source = Source::unknown("1 +\\\n 2 \\\n*\\\n 3\\\n < 874\\\n / 78 \\\n||\\\n 7\\\n % 4 \\\n== 3 \\\n&& \\\n7 ==\\\n 1");
        let mut parser = Parser::new(source);
        let ast = parser
            .binary_operation(Parser::next_value)
            .expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                lexeme: "1",
                                parsed: 1.into(),
                            })),
                            op: Plus,
                            right: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    lexeme: "2",
                                    parsed: 2.into(),
                                })),
                                op: Times,
                                right: Box::new(Expr::Literal(Literal {
                                    lexeme: "3",
                                    parsed: 3.into(),
                                })),
                            })),
                        })),
                        op: Less,
                        right: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                lexeme: "874",
                                parsed: 874.into(),
                            })),
                            op: Divide,
                            right: Box::new(Expr::Literal(Literal {
                                lexeme: "78",
                                parsed: 78.into(),
                            })),
                        })),
                    })),
                    op: Or,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                lexeme: "7",
                                parsed: 7.into(),
                            })),
                            op: Modulo,
                            right: Box::new(Expr::Literal(Literal {
                                lexeme: "4",
                                parsed: 4.into(),
                            })),
                        })),
                        op: EqualEqual,
                        right: Box::new(Expr::Literal(Literal {
                            lexeme: "3",
                            parsed: 3.into(),
                        })),
                    })),
                })),
                op: And,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        lexeme: "7",
                        parsed: 7.into(),
                    })),
                    op: EqualEqual,
                    right: Box::new(Expr::Literal(Literal {
                        lexeme: "1",
                        parsed: 1.into(),
                    })),
                })),
            })
        )
    }

    #[test]
    fn unterminated_expr() {
        let content = "(1 + 2 * )";
        let source = Source::unknown(content);
        let mut parser = Parser::new(source);
        let result = parser.binary_operation(Parser::next_value);
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
        let ast = parser
            .binary_operation(Parser::next_value)
            .expect("parsing error");
        assert_eq!(
            ast,
            Expr::Parenthesis(Parenthesis {
                expression: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        lexeme: "1",
                        parsed: 1.into(),
                    })),
                    op: Plus,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            lexeme: "2",
                            parsed: 2.into(),
                        })),
                        op: Times,
                        right: Box::new(Expr::Literal(Literal {
                            lexeme: "3",
                            parsed: 3.into(),
                        })),
                    })),
                }))
            })
        )
    }

    #[test]
    fn exitcode_operators() {
        let source = Source::unknown("(echo hello && echo world ) || echo damn");
        let mut parser = Parser::new(source);
        let ast = parser
            .binary_operation(Parser::next_statement)
            .expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Subshell(Subshell {
                    expressions: vec![Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Call(Call {
                            arguments: vec![
                                Expr::Literal("echo".into()),
                                Expr::Literal("hello".into()),
                            ],
                            tparams: vec![],
                        })),
                        op: And,
                        right: Box::new(Expr::Call(Call {
                            arguments: vec![
                                Expr::Literal("echo".into()),
                                Expr::Literal("world".into()),
                            ],
                            tparams: vec![],
                        })),
                    })]
                })),
                op: Or,
                right: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("echo".into()), Expr::Literal("damn".into())],
                    tparams: vec![],
                })),
            })
        )
    }

    #[test]
    fn escaped_operators() {
        let source = Source::unknown("(echo hello \\&& world \\);) || echo damn");
        let mut parser = Parser::new(source);
        let ast = parser
            .binary_operation(Parser::next_statement)
            .expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Subshell(Subshell {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal("echo".into()),
                            Expr::Literal("hello".into()),
                            Expr::Literal(Literal {
                                lexeme: "\\&&",
                                parsed: "&&".into(),
                            }),
                            Expr::Literal("world".into()),
                            Expr::Literal(Literal {
                                lexeme: "\\)",
                                parsed: ")".into(),
                            }),
                        ],
                        tparams: vec![],
                    })]
                })),
                op: Or,
                right: Box::new(Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            lexeme: "echo",
                            parsed: "echo".into(),
                        }),
                        Expr::Literal(Literal {
                            lexeme: "damn",
                            parsed: "damn".into(),
                        }),
                    ],
                    tparams: vec![],
                })),
            })
        )
    }
}
