use lexer::token::TokenType;
use crate::ast::Expr;
use crate::ast::group::{Block, Parenthesis};

use crate::moves::{eox, of_type, repeat, repeat_n, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};

///A parser aspect for parsing block expressions
pub trait GroupParser<'a> {
    ///Attempts to parse next group expression (a block or parenthesis expression).
    fn group(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> GroupParser<'a> for Parser<'a> {
    fn group(&mut self) -> ParseResult<Expr<'a>> {
        let pivot = self.cursor.peek().token_type;
        match pivot {
            // start of a parenthesis expression ('(')
            TokenType::RoundedLeftBracket => Ok(Expr::Parenthesis(Parenthesis {
                expressions: self.sub_exprs(TokenType::RoundedRightBracket)?,
            })),

            // start of a block expression ('{')
            TokenType::CurlyLeftBracket => Ok(Expr::Block(Block {
                expressions: self.sub_exprs(TokenType::CurlyRightBracket)?,
            })),

            _ => self.expected("unknown start of group expression"),
        }
    }
}

impl<'a> Parser<'a> {
    ///parses sub expressions of a grouping expression
    fn sub_exprs(&mut self, eog: TokenType) -> ParseResult<Vec<Expr<'a>>> {
        self.cursor.next()?; //consume group start token

        let mut expressions: Vec<Expr<'a>> = Vec::new();

        //consume all heading spaces and end of expressions (\n or ;)
        self.cursor.advance(repeat(spaces().then(eox())));

        //if we directly hit end of group, return an empty block.
        if self.cursor.advance(of_type(eog)).is_some() {
            return Ok(expressions);
        }

        loop {
            let expression = self.parse_next(eox().or(of_type(eog)))?;
            expressions.push(expression);

            //expects at least one newline or ';'
            let eox_res = self.cursor.force(
                repeat_n(1, spaces().then(eox())),
                "expected new line or semicolon",
            );

            //checks if this group expression is closed after the parsed expression
            let closed = self.cursor.advance(spaces().then(of_type(eog))).is_some();

            //if the group is closed, then we stop looking for other expressions.
            if closed {
                break;
            }
            //but if not closed, expect the cursor to hit EOX.
            eox_res?;
        }
        Ok(expressions)
    }
}

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
    use lexer::token::{Token, TokenType};

    use crate::aspects::group_parser::GroupParser;
    use crate::ast::callable::Call;
    use crate::ast::group::{Block, Parenthesis};
    use crate::ast::literal::LiteralValue::{Float, Int};
    use crate::ast::literal::{Literal};

    use crate::ast::variable::{TypedVariable, VarDeclaration, VarKind};
    use crate::ast::Expr;
    use crate::parser::Parser;
    use pretty_assertions::assert_eq;

    //noinspection DuplicatedCode
    #[test]
    fn test_empty_blocks() {
        let tokens = lex("{{{}; {}}}");
        let mut parser = Parser::new(tokens);
        let ast = parser.group().expect("failed to parse block");
        assert!(parser.cursor.is_at_end());
        assert_eq!(
            ast,
            Expr::Block(Block {
                expressions: vec![Expr::Block(Block {
                    expressions: vec![
                        Expr::Block(Block {
                            expressions: vec![]
                        }),
                        Expr::Block(Block {
                            expressions: vec![]
                        }),
                    ]
                })]
            })
        );
    }

    //noinspection DuplicatedCode
    #[test]
    fn test_empty_blocks_empty_content() {
        let tokens = lex("{;;{;;;{;;}; {\n\n};}}");
        let mut parser = Parser::new(tokens);
        let ast = parser.group().expect("failed to parse block");
        assert!(parser.cursor.is_at_end());
        assert_eq!(
            ast,
            Expr::Block(Block {
                expressions: vec![Expr::Block(Block {
                    expressions: vec![
                        Expr::Block(Block {
                            expressions: vec![]
                        }),
                        Expr::Block(Block {
                            expressions: vec![]
                        }),
                    ]
                })]
            })
        );
    }

    #[test]
    fn test_block_not_ended() {
        let tokens = lex("{ val test = 2 ");
        let mut parser = Parser::new(tokens);
        parser.group().expect_err("block parse did not failed");
    }

    #[test]
    fn test_neighbour_blocks() {
        let tokens = lex("{ {} {} }");
        let mut parser = Parser::new(tokens);
        parser.group().expect_err("block parse did not failed");
    }

    #[test]
    fn test_block_not_started() {
        let tokens = lex(" val test = 2 }");
        let mut parser = Parser::new(tokens);
        parser.group().expect_err("block parse did not failed");
    }

    #[test]
    fn test_block_with_nested_blocks() {
        let tokens = lex("\
        {\
            val test = {\
                val x = 8\n\n\n
                8
            }\n\
            ( val x = 89; command call; 7 )\
        }\
        ");
        let mut parser = Parser::new(tokens);
        let ast = parser
            .group()
            .expect("failed to parse block with nested blocks");
        assert!(parser.cursor.is_at_end());
        assert_eq!(
            ast,
            Expr::Block(Block {
                expressions: vec![
                    Expr::VarDeclaration(VarDeclaration {
                        kind: VarKind::Val,
                        var: TypedVariable {
                            name: Token::new(TokenType::Identifier, "test"),
                            ty: None,
                        },
                        initializer: Some(Box::from(Expr::Block(Block {
                            expressions: vec![
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
                    Expr::Parenthesis(Parenthesis {
                        expressions: vec![
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
                            Expr::Call(Call {
                                arguments: vec![
                                    Expr::Literal(Literal {
                                        token: Token::new(TokenType::Identifier, "command"),
                                        parsed: "command".into(),
                                    }),
                                    Expr::Literal(Literal {
                                        token: Token::new(TokenType::Identifier, "call"),
                                        parsed: "call".into(),
                                    }),
                                ],
                            }),
                            Expr::Literal(Literal {
                                token: Token::new(TokenType::IntLiteral, "7"),
                                parsed: Int(7),
                            })
                        ]
                    }),
                ]
            })
        )
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
        let ast = parser.group().expect("failed to parse block");
        assert!(parser.cursor.is_at_end());
        assert_eq!(
            ast,
            Expr::Block(Block {
                expressions: vec![
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
            })
        )
    }
}
