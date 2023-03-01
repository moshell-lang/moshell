use lexer::token::TokenType;

use crate::ast::statement::Block;
use crate::ast::Expr;
use crate::moves::{eox, of_type, repeat, repeat_n, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};

///A parser aspect for parsing block expressions
pub trait BlockParser<'a> {
    ///Attempts to parse next block expression.
    fn block(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> BlockParser<'a> for Parser<'a> {
    fn block(&mut self) -> ParseResult<Expr<'a>> {
        self.cursor.force(
            of_type(TokenType::CurlyLeftBracket),
            "expected start of block expression",
        )?;
        let block = |exprs| Ok(Expr::Block(Block { exprs }));

        let mut expressions: Vec<Expr<'a>> = Vec::new();

        //consume all heading spaces and end of expressions (\n or ;)
        self.cursor.advance(repeat(spaces().then(eox())));

        //if we directly hit end of block, return an empty block.
        if self
            .cursor
            .advance(of_type(TokenType::CurlyRightBracket))
            .is_some()
        {
            return block(expressions);
        }

        loop {
            let expression = self.statement()?;
            expressions.push(expression);

            //expects at least one newline or ;
            let eox_res = self.cursor.force(
                repeat_n(1, spaces().then(eox())),
                "expected new line or semicolon",
            );

            //checks if this block expression is closed after the parsed expression
            let closed = self
                .cursor
                .advance(spaces().then(of_type(TokenType::CurlyRightBracket)))
                .is_some();

            //if the block is closed, then we stop looking for other expressions.
            if closed {
                break;
            }
            //but if not closed, expect the cursor to hit EOX.
            eox_res?;
        }
        block(expressions)
    }
}

#[cfg(test)]
mod tests {
    use lexer::token::{Token, TokenType};

    use crate::aspects::block_parser::BlockParser;
    use crate::ast::callable::Call;
    use crate::ast::literal::Literal;
    use crate::ast::literal::LiteralValue::{Float, Int};
    use crate::ast::statement::Block;
    use crate::ast::variable::{TypedVariable, VarDeclaration, VarKind};
    use crate::ast::Expr;
    use crate::parser::Parser;
    use crate::source::Source;
    use pretty_assertions::assert_eq;

    //noinspection DuplicatedCode
    #[test]
    fn test_empty_blocks() {
        let source = Source::unknown("{{{}; {}}}");
        let mut parser = Parser::new(source);
        let ast = parser.block().expect("failed to parse block");
        assert!(parser.cursor.is_at_end());
        assert_eq!(
            ast,
            Expr::Block(Block {
                exprs: vec![Expr::Block(Block {
                    exprs: vec![
                        Expr::Block(Block { exprs: vec![] }),
                        Expr::Block(Block { exprs: vec![] }),
                    ]
                })]
            })
        );
    }

    //noinspection DuplicatedCode
    #[test]
    fn test_empty_blocks_empty_content() {
        let source = Source::unknown("{;;{;;;{;;}; {\n\n};}}");
        let mut parser = Parser::new(source);
        let ast = parser.block().expect("failed to parse block");
        assert!(parser.cursor.is_at_end());
        assert_eq!(
            ast,
            Expr::Block(Block {
                exprs: vec![Expr::Block(Block {
                    exprs: vec![
                        Expr::Block(Block { exprs: vec![] }),
                        Expr::Block(Block { exprs: vec![] }),
                    ]
                })]
            })
        );
    }

    #[test]
    fn test_block_not_ended() {
        let source = Source::unknown("{ val test = 2 ");
        let mut parser = Parser::new(source);
        parser.block().expect_err("block parse did not failed");
    }

    #[test]
    fn test_neighbour_blocks() {
        let source = Source::unknown("{ {} {} }");
        let mut parser = Parser::new(source);
        parser.block().expect_err("block parse did not failed");
    }

    #[test]
    fn test_block_not_started() {
        let source = Source::unknown(" val test = 2 }");
        let mut parser = Parser::new(source);
        parser.block().expect_err("block parse did not failed");
    }

    #[test]
    fn test_block_with_nested_blocks() {
        let source = Source::unknown(
            "\
        {\
            val test = {\
                val x = 8\n\n\n
                8
            }\n\
            { val x = 89; var test = 77; command call; }\
        }\
        ",
        );
        let mut parser = Parser::new(source);
        let ast = parser
            .block()
            .expect("failed to parse block with nested blocks");
        assert!(parser.cursor.is_at_end());
        assert_eq!(
            ast,
            Expr::Block(Block {
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
                                        lexme: "8",
                                        parsed: Int(8),
                                    }))),
                                }),
                                Expr::Literal(Literal {
                                    lexme: "8",
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
                                    lexme: "89",
                                    parsed: Int(89),
                                }))),
                            }),
                            Expr::VarDeclaration(VarDeclaration {
                                kind: VarKind::Var,
                                var: TypedVariable {
                                    name: Token::new(TokenType::Identifier, "test"),
                                    ty: None,
                                },
                                initializer: Some(Box::from(Expr::Literal(Literal {
                                    lexme: "77",
                                    parsed: Int(77),
                                }))),
                            }),
                            Expr::Call(Call {
                                arguments: vec![
                                    Expr::Literal("command".into()),
                                    Expr::Literal("call".into()),
                                ],
                            }),
                        ]
                    }),
                ]
            })
        )
    }

    #[test]
    fn test_block() {
        let source = Source::unknown(
            "\
        {\
            var test: int = 7.0\n\
            val x = 8\
        }\
        ",
        );
        let mut parser = Parser::new(source);
        let ast = parser.block().expect("failed to parse block");
        assert!(parser.cursor.is_at_end());
        assert_eq!(
            ast,
            Expr::Block(Block {
                exprs: vec![
                    Expr::VarDeclaration(VarDeclaration {
                        kind: VarKind::Var,
                        var: TypedVariable {
                            name: Token::new(TokenType::Identifier, "test"),
                            ty: Some(Token::new(TokenType::Identifier, "int")),
                        },
                        initializer: Some(Box::new(Expr::Literal(Literal {
                            lexme: "7.0",
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
                            lexme: "8",
                            parsed: Int(8),
                        }))),
                    }),
                ]
            })
        )
    }
}