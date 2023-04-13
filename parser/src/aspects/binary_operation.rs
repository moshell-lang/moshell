use crate::moves::{bin_op, eox, spaces, MoveOperations};
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
            .lookahead(spaces().then(bin_op()))
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
        let operator = self.cursor.advance(spaces().then(bin_op())).map(|t| {
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
            .lookahead(spaces().then(bin_op()))
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

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use context::source::{Source, SourceSegmentHolder};

    use crate::aspects::binary_operation::BinaryOperationsAspect;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parser::Parser;
    use crate::source::{literal, literal_nth};
    use ast::call::Call;
    use ast::group::{Parenthesis, Subshell};
    use ast::operation::BinaryOperation;
    use ast::operation::BinaryOperator::*;
    use ast::value::Literal;
    use ast::Expr;
    use context::str_find::{find_between, find_in, find_in_nth};

    #[test]
    fn is_left_associative() {
        let source = Source::unknown("1 && 2 || \\\n 3 || 4 && 5");
        let mut parser = Parser::new(source.clone());
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
                                parsed: 1.into(),
                                segment: 0..1
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
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 4.into(),
                        segment: find_in(source.source, "4")
                    })),
                })),
                op: And,
                right: Box::new(Expr::Literal(Literal {
                    parsed: 5.into(),
                    segment: find_in(source.source, "5")
                })),
            }),
        )
    }

    #[test]
    fn explicit_priority() {
        let source = Source::unknown("1 \\\n+\\\n (2 + 3)");
        let mut parser = Parser::new(source.clone());
        let ast = parser
            .binary_operation(Parser::next_value)
            .expect("parsing error");
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
        let mut parser = Parser::new(source.clone());
        let ast = parser
            .binary_operation(Parser::next_value)
            .expect("parsing error");
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
        let mut parser = Parser::new(source.clone());
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
                        segment: source.source.rfind('1').map(|p| (p..p + 1)).unwrap()
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
        let mut parser = Parser::new(source.clone());
        let ast = parser
            .binary_operation(Parser::next_value)
            .expect("parsing error");
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
        let mut parser = Parser::new(source.clone());
        let ast = parser
            .binary_operation(Parser::next_statement)
            .expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Subshell(Subshell {
                    expressions: vec![Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Call(Call {
                            path: Vec::new(),
                            arguments: vec![
                                literal(source.source, "echo"),
                                literal(source.source, "hello"),
                            ],
                            type_parameters: vec![],
                        })),
                        op: And,
                        right: Box::new(Expr::Call(Call {
                            path: Vec::new(),
                            arguments: vec![
                                literal_nth(source.source, "echo", 1),
                                literal(source.source, "world"),
                            ],
                            type_parameters: vec![],
                        })),
                    })],
                    segment: find_between(source.source, "(", ")")
                })),
                op: Or,
                right: Box::new(Expr::Call(Call {
                    path: Vec::new(),
                    arguments: vec![
                        literal_nth(source.source, "echo", 2),
                        literal(source.source, "damn"),
                    ],
                    type_parameters: vec![],
                })),
            })
        )
    }

    #[test]
    fn escaped_operators() {
        let source = Source::unknown("(echo hello \\& world \\);) || echo damn");
        let mut parser = Parser::new(source.clone());
        let ast = parser
            .binary_operation(Parser::next_statement)
            .expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Subshell(Subshell {
                    expressions: vec![Expr::Call(Call {
                        path: Vec::new(),
                        arguments: vec![
                            literal(source.source, "echo"),
                            literal(source.source, "hello"),
                            literal(source.source, "&"),
                            literal(source.source, "world"),
                            literal(source.source, ")"),
                        ],
                        type_parameters: vec![],
                    })],
                    segment: find_between(source.source, "(", ";)")
                })),
                op: Or,
                right: Box::new(Expr::Call(Call {
                    path: Vec::new(),
                    arguments: vec![
                        literal_nth(source.source, "echo", 1),
                        literal(source.source, "damn"),
                    ],
                    type_parameters: vec![],
                })),
            })
        )
    }
}
