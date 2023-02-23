use lexer::token::{Token, TokenType};
use crate::ast::Expr;
use crate::ast::operation::{BinaryOperation, BinaryOperator};
use crate::moves::{bin_op, MoveOperations, spaces};
use crate::parser::{Parser, ParseResult};

pub(crate) trait BinaryOps<'p> {
    fn value_expression(&mut self) -> ParseResult<Expr<'p>>;
}

fn to_bin_operator(token: Token) -> BinaryOperator {
    match token.token_type {
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
    fn value_expression(&mut self) -> ParseResult<Expr<'p>> {
        let left = self.expression()?;
        let operator = self.cursor.advance(spaces().then(bin_op()));

        if let Some(operator) = operator {
            let right = self.expression()?;

            return Ok(Expr::Binary(BinaryOperation {
                left: Box::new(left),
                op: to_bin_operator(operator),
                right: Box::new(right),
            }))
        }

        return Ok(left);
    }
}

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
    use lexer::token::{Token, TokenType};
    use crate::ast::callable::Call;
    use crate::ast::Expr;
    use crate::ast::group::Parenthesis;
    use crate::ast::literal::{Literal, LiteralValue};
    use crate::ast::operation::{BinaryOperation, BinaryOperator};
    use crate::parse;

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
}
