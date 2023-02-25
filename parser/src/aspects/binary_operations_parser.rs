use lexer::token::TokenType;

use crate::ast::Expr;
use crate::ast::operation::{BinaryOperation, BinaryOperator};
use crate::moves::{bin_op, Move, MoveOperations, spaces};
use crate::parser::{Parser, ParseResult};

pub(crate) trait BinaryOps<'p> {
    fn binary_operator(&mut self, eox: impl Move + Copy) -> ParseResult<Expr<'p>>;

    fn binary_operator_right(&mut self, left: Expr<'p>, eox: impl Move + Copy) -> ParseResult<Expr<'p>>;
}

fn to_bin_operator(token_type: TokenType) -> BinaryOperator {
    match token_type {
        TokenType::And => BinaryOperator::And,
        TokenType::Or => BinaryOperator::Or,

        TokenType::EqualEqual => BinaryOperator::EqualEqual,
        TokenType::NotEqual => BinaryOperator::NotEqual,
        TokenType::Less => BinaryOperator::Less,
        TokenType::LessEqual => BinaryOperator::LessEqual,
        TokenType::Greater => BinaryOperator::Greater,
        TokenType::GreaterEqual => BinaryOperator::GreaterEqual,

        TokenType::Plus => BinaryOperator::Plus,
        TokenType::Minus => BinaryOperator::Minus,
        TokenType::Star => BinaryOperator::Times,
        TokenType::Slash => BinaryOperator::Divide,
        TokenType::Percent => BinaryOperator::Modulo,
        _ => panic!("unexpected non-binary operator token.")
    }
}

impl<'p> BinaryOps<'p> for Parser<'p> {
    fn binary_operator(&mut self, eox: impl Move + Copy) -> ParseResult<Expr<'p>> {
        let left = self.statement()?;
        self.binary_operator_right(left, eox)
    }

    fn binary_operator_right(&mut self, left: Expr<'p>, eox: impl Move + Copy) -> ParseResult<Expr<'p>> {
        self.binary_op_right_internal(left, eox, i8::MIN)
    }
}

impl<'p> Parser<'p> {
    fn binary_op_right_internal(&mut self, left: Expr<'p>, eox: impl Move + Copy, priority: i8) -> ParseResult<Expr<'p>> {
        let mut operation = self.binary_operation_internal(left, eox)?;
        macro_rules! hit_eox {
            () => { self.cursor.lookahead(spaces().then(eox)).is_some() };
        }

        while !hit_eox!() && self.has_priority(priority) {
            operation = self.binary_operation_internal(operation, eox)?;
        }

        Ok(operation)
    }

    fn has_priority(&self, current_priority: i8) -> bool {
        self.cursor
            .lookahead(spaces().then(bin_op()))
            .map(|t| (current_priority.saturating_sub(to_bin_operator(t.token_type).priority())))
            .map(|comp| comp <= 0)
            .unwrap_or(false)
    }

    fn binary_operation_internal(&mut self, left: Expr<'p>, eox: impl Move + Copy) -> ParseResult<Expr<'p>>
    {
        //current expressions' infix operator
        let operator = self.cursor
            .advance(spaces().then(bin_op()))
            .map(|t| to_bin_operator(t.token_type));

        if let Some(operator) = operator {
            let operator_priority = operator.priority();
            let right = self.statement()?;

            let priority_comparison = self.cursor
                .lookahead(spaces().then(bin_op()))
                .map(|t| to_bin_operator(t.token_type).priority() - operator_priority)
                .unwrap_or_default();

            if priority_comparison < 0 {
                return Ok(Expr::Binary(BinaryOperation {
                    left: Box::new(left),
                    op: operator,
                    right: Box::new(right),
                }))
            }

            //current and next operator have same priority
            if priority_comparison == 0 {
                return self.binary_op_right_internal(Expr::Binary(BinaryOperation {
                    left: Box::new(left),
                    op: operator,
                    right: Box::new(right),
                }), eox, operator_priority);
            }

            return Ok(Expr::Binary(BinaryOperation {
                left: Box::new(left),
                op: operator,
                right: Box::new(self.binary_op_right_internal(right, eox, operator_priority)?),
            }));
        }

        return Ok(left);
    }
}


#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use lexer::lexer::lex;
    use lexer::token::{Token, TokenType};

    use crate::aspects::binary_operations_parser::right_pivot;
    use crate::ast::Expr;
    use crate::ast::group::Parenthesis;
    use crate::ast::literal::{Literal, LiteralValue};
    use crate::ast::operation::{BinaryOperation, BinaryOperator};
    use crate::parse;

    #[test]
    fn is_left_associative() {
        let tokens = lex("1 + 2 + 3");
        let ast = parse(tokens).expect("parsing error");
        assert_eq!(
            ast,
            vec![
                Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            token: Token::new(TokenType::IntLiteral, "1"),
                            parsed: LiteralValue::Int(1),
                        })),
                        op: BinaryOperator::Plus,
                        right: Box::new(Expr::Literal(Literal {
                            token: Token::new(TokenType::IntLiteral, "2"),
                            parsed: LiteralValue::Int(2),
                        })),
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::Literal(Literal {
                        token: Token::new(TokenType::IntLiteral, "3"),
                        parsed: LiteralValue::Int(3),
                    })),
                }),
            ]
        )
    }

    #[test]
    fn explicit_priority() {
        let tokens = lex("1 + (2 + 3)");
        let ast = parse(tokens).expect("parsing error");
        assert_eq!(
            ast,
            vec![
                Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        token: Token::new(TokenType::IntLiteral, "1"),
                        parsed: LiteralValue::Int(1),
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::Parenthesis(Parenthesis {
                        expressions: vec![
                            Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    token: Token::new(TokenType::IntLiteral, "2"),
                                    parsed: LiteralValue::Int(2),
                                })),
                                op: BinaryOperator::Plus,
                                right: Box::new(Expr::Literal(Literal {
                                    token: Token::new(TokenType::IntLiteral, "3"),
                                    parsed: LiteralValue::Int(3),
                                })),
                            }),
                        ]
                    })),
                })
            ]
        )
    }

    #[test]
    fn arithmetic_priority() {
        let tokens = lex("1 + 2 * 3");
        let ast = parse(tokens).expect("parsing error");
        assert_eq!(
            ast,
            vec![
                Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        token: Token::new(TokenType::IntLiteral, "1"),
                        parsed: LiteralValue::Int(1),
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            token: Token::new(TokenType::IntLiteral, "2"),
                            parsed: LiteralValue::Int(2),
                        })),
                        op: BinaryOperator::Times,
                        right: Box::new(Expr::Literal(Literal {
                            token: Token::new(TokenType::IntLiteral, "3"),
                            parsed: LiteralValue::Int(3),
                        })),
                    })),
                })
            ]
        )
    }

    #[test]
    fn complete_prioritization_test() {
        let tokens = lex("1 + 2 * 3 < 874 * 78 || 7 - 4 == 3 && 7 == 1");
        let ast = parse(tokens).expect("parsing error");
        assert_eq!(
            ast,
            vec![
                Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    token: Token::new(TokenType::IntLiteral, "1"),
                                    parsed: LiteralValue::Int(1),
                                })),
                                op: BinaryOperator::Plus,
                                right: Box::new(Expr::Binary(BinaryOperation {
                                    left: Box::new(Expr::Literal(Literal {
                                        token: Token::new(TokenType::IntLiteral, "2"),
                                        parsed: LiteralValue::Int(2),
                                    })),
                                    op: BinaryOperator::Times,
                                    right: Box::new(Expr::Literal(Literal {
                                        token: Token::new(TokenType::IntLiteral, "3"),
                                        parsed: LiteralValue::Int(3),
                                    })),
                                })),
                            })),
                            op: BinaryOperator::Less,
                            right: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    token: Token::new(TokenType::IntLiteral, "874"),
                                    parsed: LiteralValue::Int(874),
                                })),
                                op: BinaryOperator::Times,
                                right: Box::new(Expr::Literal(Literal {
                                    token: Token::new(TokenType::IntLiteral, "78"),
                                    parsed: LiteralValue::Int(78),
                                })),
                            })),
                        })),
                        op: BinaryOperator::Or,
                        right: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    token: Token::new(TokenType::IntLiteral, "7"),
                                    parsed: LiteralValue::Int(7),
                                })),
                                op: BinaryOperator::Minus,
                                right: Box::new(Expr::Literal(Literal {
                                    token: Token::new(TokenType::IntLiteral, "4"),
                                    parsed: LiteralValue::Int(4),
                                })),
                            })),
                            op: BinaryOperator::EqualEqual,
                            right: Box::new(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "3"),
                                parsed: LiteralValue::Int(3),
                            })),
                        })),
                    })),
                    op: BinaryOperator::And,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Literal(Literal {
                            token: Token::new(TokenType::IntLiteral, "7"),
                            parsed: LiteralValue::Int(7),
                        })),
                        op: BinaryOperator::EqualEqual,
                        right: Box::new(Expr::Literal(Literal {
                            token: Token::new(TokenType::IntLiteral, "1"),
                            parsed: LiteralValue::Int(1),
                        }))
                    }))
                })
                ,

            ]
        )
    }

    /* //FIXME wait for PR#15 to be merged
        #[test]
        fn test_exitcode_operators() {
            let tokens = lex("(echo hello && echo world) || echo damn");
            let ast = parse(tokens).expect("parsing error");
            assert_eq!(
                ast,
                vec![
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
                                    op: BinaryOperator::And,
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
                        op: BinaryOperator::Or,
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
                ]
            )
        }
     */
}
