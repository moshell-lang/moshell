
use crate::ast::Expr;
use crate::ast::operation::{BinaryOperation, BinaryOperator};
use crate::context::ParserContext;
use crate::moves::{bin_op, eox, MoveOperations, spaces};
use crate::parser::{Parser, ParseResult};


/// a parser aspect to parse any kind of binary operations
pub trait BinaryOperationsParser<'p> {
    ///Parses a binary operation tree
    /// `eox`: a selector to define where the binary operation expression hits it end.
    fn binary_operation(&mut self,
                        ctx: ParserContext,
    ) -> ParseResult<Expr<'p>>;

    ///Parses the operator and left arm of a left-defined operation.
    /// `left`: the left arm of the binary operator.
    /// `eox`: a selector to define where the binary operation expression hits it end.
    fn binary_operation_right(&mut self,
                              left: Expr<'p>,
                              ctx: ParserContext,
    ) -> ParseResult<Expr<'p>>;
}




impl<'p> BinaryOperationsParser<'p> for Parser<'p> {
    fn binary_operation(&mut self,
                        ctx: ParserContext,
    ) -> ParseResult<Expr<'p>> {
        let left = self.statement(ctx.clone())?;
        self.binary_operation_right(left, ctx)
    }

    fn binary_operation_right(&mut self,
                              left: Expr<'p>,
                              ctx: ParserContext,
    ) -> ParseResult<Expr<'p>> {
        //parsing a top-level tree operation with fewest priority
        self.binary_op_right_internal(i8::MIN, ctx, left)
    }
}

impl<'p> Parser<'p> {
    fn ensure_in_ops(&self, op: BinaryOperator, ctx: ParserContext) -> ParseResult<BinaryOperator> {
        if ctx.allowed_operators.contains(&op) {
            return Ok(op)
        }
        self.expected("unexpected binary operator")
    }

    //Parses a binary operation tree as long as it does not hits an operation with smaller priority.
    fn binary_op_right_internal(&mut self,
                                priority: i8,
                                ctx: ParserContext,
                                left: Expr<'p>,
    ) -> ParseResult<Expr<'p>> {
        let mut operation = self.binary_operation_internal(left, ctx.clone())?;
        macro_rules! hit_eox {
            () => { self.cursor.lookahead(spaces().then(eox())).is_some() };
        }

        while !hit_eox!() && self.has_priority(priority) {
            operation = self.binary_operation_internal(operation, ctx.clone())?;
        }

        Ok(operation)
    }

    //does current operator priority has priority over next binary operator ?
    fn has_priority(&self, current_priority: i8) -> bool {
        self.cursor
            .lookahead(spaces().then(bin_op()))
            .map(|t| BinaryOperator::convert_bin_operator(t.token_type)
                .expect("conception error") //cannot fail
                .priority()
                .saturating_sub(current_priority))
            .map(|comp| comp > 0)
            .unwrap_or(false)
    }

    fn binary_operation_internal(&mut self,
                                 left: Expr<'p>,
                                 ctx: ParserContext,
    ) -> ParseResult<Expr<'p>> {
        //current expressions' infix operator
        let operator = self.cursor
            .advance(spaces().then(bin_op()))
            .map(|t| BinaryOperator::convert_bin_operator(t.token_type) //cannot fail
                .expect("conception error"));

        if operator.is_none() {
            return Ok(left)
        }

        let operator = operator.unwrap();
        self.ensure_in_ops(operator, ctx.clone())?;

        let operator_priority = operator.priority();
        //directly-next statement
        let right = self.statement(ctx.clone())?;

        //comparison between current operator's priority and next operator (if any)
        //is 0 if priorities are same or if there is no next operator.
        //is < 0 if current operator's priority is higher
        //is > 0 if current operator's priority is smaller
        let priority_comparison = self.cursor
            .lookahead(spaces().then(bin_op()))
            .map(|t| operator_priority - BinaryOperator::convert_bin_operator(t.token_type)
                .expect("conception error") //cannot fail
                .priority())
            .unwrap_or(0);


        let result = if priority_comparison > 0 {
            //current binary operator has more priority so we directly return it
            Expr::Binary(BinaryOperation {
                left: Box::new(left),
                op: operator,
                right: Box::new(right),
            })
        } else if priority_comparison == 0 {
            //same priority so we can continue to parse to the right,
            // passing current binary operation as the left operation.
            self.binary_op_right_internal(operator_priority, ctx, Expr::Binary(BinaryOperation {
                left: Box::new(left),
                op: operator,
                right: Box::new(right),
            }))?
        } else {
            //priority is fewer, so we let the priority to the right by continuing to parse
            Expr::Binary(BinaryOperation {
                left: Box::new(left),
                op: operator,
                right: Box::new(self.binary_op_right_internal(operator_priority, ctx, right)?),
            })
        };

        return Ok(result);
    }
}


#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use lexer::lexer::lex;
    use lexer::token::{Token, TokenType};

    use crate::aspects::binary_operation_parser::BinaryOperationsParser;
    use crate::ast::callable::Call;
    use crate::ast::Expr;
    use crate::ast::group::Parenthesis;
    use crate::ast::literal::{Literal, LiteralValue};
    use crate::ast::operation::BinaryOperation;
    use crate::ast::operation::BinaryOperator::*;
    use crate::context::ParserContext;
    use crate::parser::Parser;

    #[test]
    fn is_left_associative() {
        let tokens = lex("1 && 2 || 3 || 4 && 5");
        let ast = Parser::new(tokens).binary_operation(ParserContext::default()).expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "1"),
                                parsed: LiteralValue::Int(1),
                            })),
                            op: And,
                            right: Box::new(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "2"),
                                parsed: LiteralValue::Int(2),
                            })),
                        })),
                        op: Or,
                        right: Box::new(Expr::Literal(Literal {
                            token: Token::new(TokenType::IntLiteral, "3"),
                            parsed: LiteralValue::Int(3),
                        })),
                    })),
                    op: Or,
                    right: Box::new(Expr::Literal(Literal {
                        token: Token::new(TokenType::IntLiteral, "4"),
                        parsed: LiteralValue::Int(4),
                    })),
                })),
                op: And,
                right: Box::new(Expr::Literal(Literal {
                    token: Token::new(TokenType::IntLiteral, "5"),
                    parsed: LiteralValue::Int(5),
                })),
            }),
        )
    }

    #[test]
    fn explicit_priority() {
        let tokens = lex("1 + (2 + 3)");
        let ast = Parser::new(tokens).binary_operation(ParserContext::value_hold()).expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Literal(Literal {
                    token: Token::new(TokenType::IntLiteral, "1"),
                    parsed: LiteralValue::Int(1),
                })),
                op: Plus,
                right: Box::new(Expr::Parenthesis(Parenthesis {
                    expressions: vec![
                        Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "2"),
                                parsed: LiteralValue::Int(2),
                            })),
                            op: Plus,
                            right: Box::new(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "3"),
                                parsed: LiteralValue::Int(3),
                            })),
                        }),
                    ]
                })),
            })
        )
    }

    #[test]
    fn arithmetic_priority() {
        let tokens = lex("1 + 2 * 3");
        let ast = Parser::new(tokens).binary_operation(ParserContext::value_hold()).expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Literal(Literal {
                    token: Token::new(TokenType::IntLiteral, "1"),
                    parsed: LiteralValue::Int(1),
                })),
                op: Plus,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        token: Token::new(TokenType::IntLiteral, "2"),
                        parsed: LiteralValue::Int(2),
                    })),
                    op: Times,
                    right: Box::new(Expr::Literal(Literal {
                        token: Token::new(TokenType::IntLiteral, "3"),
                        parsed: LiteralValue::Int(3),
                    })),
                })),
            })
        )
    }

    #[test]
    fn complete_prioritization_test() {
        let tokens = lex("1 + 2 * 3 < 874 * 78 || 7 - 4 == 3 && 7 == 1");
        let ast = Parser::new(tokens)
            .binary_operation(ParserContext::value_hold())
            .expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "1"),
                                parsed: LiteralValue::Int(1),
                            })),
                            op: Plus,
                            right: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    token: Token::new(TokenType::IntLiteral, "2"),
                                    parsed: LiteralValue::Int(2),
                                })),
                                op: Times,
                                right: Box::new(Expr::Literal(Literal {
                                    token: Token::new(TokenType::IntLiteral, "3"),
                                    parsed: LiteralValue::Int(3),
                                })),
                            })),
                        })),
                        op: Less,
                        right: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "874"),
                                parsed: LiteralValue::Int(874),
                            })),
                            op: Times,
                            right: Box::new(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "78"),
                                parsed: LiteralValue::Int(78),
                            })),
                        })),
                    })),
                    op: Or,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "7"),
                                parsed: LiteralValue::Int(7),
                            })),
                            op: Minus,
                            right: Box::new(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "4"),
                                parsed: LiteralValue::Int(4),
                            })),
                        })),
                        op: EqualEqual,
                        right: Box::new(Expr::Literal(Literal {
                            token: Token::new(TokenType::IntLiteral, "3"),
                            parsed: LiteralValue::Int(3),
                        })),
                    })),
                })),
                op: And,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        token: Token::new(TokenType::IntLiteral, "7"),
                        parsed: LiteralValue::Int(7),
                    })),
                    op: EqualEqual,
                    right: Box::new(Expr::Literal(Literal {
                        token: Token::new(TokenType::IntLiteral, "1"),
                        parsed: LiteralValue::Int(1),
                    })),
                })),
            })
        )
    }

    #[test]
    fn bin_expression_in_group() {
        let tokens = lex("(1 + 2 * 3)");
        let ast = Parser::new(tokens).binary_operation(ParserContext::value_hold()).expect("parsing error");
        assert_eq!(
            ast,
            Expr::Parenthesis(Parenthesis {
                expressions: vec![
                    Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            token: Token::new(TokenType::IntLiteral, "1"),
                            parsed: LiteralValue::Int(1),
                        })),
                        op: Plus,
                        right: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "2"),
                                parsed: LiteralValue::Int(2),
                            })),
                            op: Times,
                            right: Box::new(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "3"),
                                parsed: LiteralValue::Int(3),
                            })),
                        })),
                    })
                ]
            })
        )
    }


    #[test]
    fn test_exitcode_operators() {
        let tokens = lex("(echo hello && echo world ) || echo damn");
        let ast = Parser::new(tokens).binary_operation(ParserContext::default()).expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Parenthesis(Parenthesis {
                    expressions: vec![
                        Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Call(Call {
                                arguments: vec![
                                    Expr::Literal(Literal {
                                        token: Token::new(TokenType::Identifier, "echo"),
                                        parsed: LiteralValue::String("echo".to_string()),
                                    }),
                                    Expr::Literal(Literal {
                                        token: Token::new(TokenType::Identifier, "hello"),
                                        parsed: LiteralValue::String("hello".to_string()),
                                    }),
                                ],
                            })),
                            op: And,
                            right: Box::new(Expr::Call(Call {
                                arguments: vec![
                                    Expr::Literal(Literal {
                                        token: Token::new(TokenType::Identifier, "echo"),
                                        parsed: LiteralValue::String("echo".to_string()),
                                    }),
                                    Expr::Literal(Literal {
                                        token: Token::new(TokenType::Identifier, "world"),
                                        parsed: LiteralValue::String("world".to_string()),
                                    }),
                                ],
                            })),
                        })
                    ]
                })),
                op: Or,
                right: Box::new(Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "echo"),
                            parsed: LiteralValue::String("echo".to_string()),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "damn"),
                            parsed: LiteralValue::String("damn".to_string()),
                        }),
                    ],
                })),
            })
        )
    }

    #[test]
    fn test_escaped_operators() {
        let tokens = lex("(echo hello \\&& world \\);) || echo damn");
        let ast = Parser::new(tokens).binary_operation(ParserContext::default()).expect("parsing error");
        assert_eq!(
            ast,
            Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Parenthesis(Parenthesis {
                    expressions: vec![
                        Expr::Call(Call {
                            arguments: vec![
                                Expr::Literal(Literal {
                                    token: Token::new(TokenType::Identifier, "echo"),
                                    parsed: LiteralValue::String("echo".to_string()),
                                }),
                                Expr::Literal(Literal {
                                    token: Token::new(TokenType::Identifier, "hello"),
                                    parsed: LiteralValue::String("hello".to_string()),
                                }),
                                Expr::Literal(Literal {
                                    token: Token::new(TokenType::BackSlash, "\\"),
                                    parsed: LiteralValue::String("&&".to_string()),
                                }),
                                Expr::Literal(Literal {
                                    token: Token::new(TokenType::Identifier, "world"),
                                    parsed: LiteralValue::String("world".to_string()),
                                }),
                                Expr::Literal(Literal {
                                    token: Token::new(TokenType::BackSlash, "\\"),
                                    parsed: LiteralValue::String(")".to_string()),
                                }),
                            ],
                        })
                    ]
                })),
                op: Or,
                right: Box::new(Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "echo"),
                            parsed: LiteralValue::String("echo".to_string()),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "damn"),
                            parsed: LiteralValue::String("damn".to_string()),
                        }),
                    ],
                })),
            })
        )
    }
}
