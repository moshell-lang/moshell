use lexer::token::TokenType;
use crate::aspects::base_parser::BaseParser;
use crate::ast::Expr;
use crate::parser::{Parser, ParseResult};
use crate::ast::statement::Block;

pub trait BlockParser<'a> {
    fn block(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> BlockParser<'a> for Parser<'a> {
    fn block(&mut self) -> ParseResult<Expr<'a>> {
        self.expect_token(TokenType::CurlyLeftBracket, "expected start of block expression")?;
        let block = |exprs| Ok(Expr::Block(Block {
            exprs
        }));
        let mut expressions: Vec<Expr<'a>> = Vec::new();
        //if the block is empty
        if self.meet_token(TokenType::CurlyRightBracket) {
            return block(expressions);
        }
        loop {
            if self.meet_token(TokenType::CurlyRightBracket) {
                break;
            }

            let expression = self.statement()?;
            expressions.push(expression);
            if self.meet_token(TokenType::CurlyRightBracket) {
                break;
            }


            self.expect_token(TokenType::SemiColon, "")
                .or_else(|_| self.expect_token(TokenType::NewLine, "expected new line or semicolon"))?;
        };
        block(expressions)
    }
}

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
    use lexer::token::{Token, TokenType};
    use crate::aspects::base_parser::BaseParser;
    use crate::aspects::block_parser::BlockParser;
    use crate::ast::Expr;
    use crate::ast::literal::Literal;
    use crate::ast::literal::LiteralValue::{Float, Int};
    use crate::ast::statement::Block;
    use crate::ast::variable::{TypedVariable, VarDeclaration, VarKind};
    use crate::parser::Parser;

    #[test]
    fn test_empty_blocks() {
        let tokens = lex("{{{}; {}}}");
        let mut parser = Parser::new(tokens);
        let ast = parser.block().expect("failed to parse block");
        assert!(parser.is_at_end());
        assert_eq!(ast, Expr::Block(Block {
            exprs: vec![Expr::Block(Block {
                exprs: vec![
                    Expr::Block(Block {
                        exprs: vec![]
                    }),
                    Expr::Block(Block {
                        exprs: vec![]
                    }),
                ]
            })]
        }));
    }

    #[test]
    fn test_block_not_ended() {
        let tokens = lex("{ val test = 2 ");
        let mut parser = Parser::new(tokens);
        parser.block().expect_err("block parse did not failed");
    }

    #[test]
    fn test_block_not_started() {
        let tokens = lex(" val test = 2 }");
        let mut parser = Parser::new(tokens);
        parser.block().expect_err("block parse did not failed");
    }

    #[test]
    fn test_block_with_nested_blocks() {
        let tokens = lex("\
        {\
            val test = {\
                val x = 8
                8
            }\n\
            { val x = 89 }\
        }\
        ");
        let mut parser = Parser::new(tokens);
        let ast = parser.block().expect("failed to parse block with nested blocks");
        assert!(parser.is_at_end());
        assert_eq!(ast, Expr::Block(Block {
            exprs: vec![
                Expr::VarDeclaration(VarDeclaration {
                    kind: VarKind::Val,
                    var: TypedVariable {
                        name: Token::new(TokenType::Identifier, "test"),
                        ty: None,
                    },
                    initializer: Some(Box::from(Expr::Block(Block {
                        exprs: vec![
                            Expr::VarDeclaration(VarDeclaration {
                                kind: VarKind::Val,
                                var: TypedVariable {
                                    name: Token::new(TokenType::Identifier, "x"),
                                    ty: None,
                                },
                                initializer: Some(Box::from(Expr::Literal(Literal {
                                    token: Token::new(TokenType::IntLiteral, "8"),
                                    parsed: Int(8),
                                }))),
                            }),
                            Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "8"),
                                parsed: Int(8),
                            }),
                        ]
                    }))),
                }),
                Expr::Block(Block {
                    exprs: vec![
                        Expr::VarDeclaration(VarDeclaration {
                            kind: VarKind::Val,
                            var: TypedVariable {
                                name: Token::new(TokenType::Identifier, "x"),
                                ty: None,
                            },
                            initializer: Some(Box::from(Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "89"),
                                parsed: Int(89),
                            }))),
                        }),
                    ]
                }),
            ]
        }))
    }

    #[test]
    fn test_block() {
        let tokens = lex("\
        {\
            var test: int = 7.0\n\
            val x = 8\
        }\
        ");
        let mut parser = Parser::new(tokens);
        let ast = parser.block().expect("failed to parse block");
        assert!(parser.is_at_end());
        assert_eq!(ast, Expr::Block(Block {
            exprs: vec![
                Expr::VarDeclaration(VarDeclaration {
                    kind: VarKind::Var,
                    var: TypedVariable {
                        name: Token::new(TokenType::Identifier, "test"),
                        ty: Some(Token::new(TokenType::Identifier, "int")),
                    },
                    initializer: Some(Box::new(Expr::Literal(Literal {
                        token: Token::new(TokenType::FloatLiteral, "7.0"),
                        parsed: Float(7.0),
                    }))),
                }),
                Expr::VarDeclaration(VarDeclaration {
                    kind: VarKind::Val,
                    var: TypedVariable {
                        name: Token::new(TokenType::Identifier, "x"),
                        ty: None,
                    },
                    initializer: Some(Box::new(Expr::Literal(Literal {
                        token: Token::new(TokenType::IntLiteral, "8"),
                        parsed: Int(8),
                    }))),
                }),
            ]
        }))
    }
}