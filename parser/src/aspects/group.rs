use lexer::token::{Token, TokenType};

use crate::ast::group::{Block, Parenthesis, Subshell};
use crate::ast::Expr;
use crate::moves::{eox, of_type, repeat, repeat_n, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};

///A parser aspect for parsing block expressions
pub trait GroupAspect<'a> {
    ///Parse a block expression.
    /// Block expressions will parse contained expressions as statements.
    /// see `Parser::statement` for further details.
    fn block(&mut self) -> ParseResult<Block<'a>>;

    ///Parse a subshell expression.
    /// subshell expressions will parse contained expressions as statements.
    /// see [`Parser::statement`] for further details.
    fn subshell(&mut self) -> ParseResult<Subshell<'a>>;

    ///Parse a parenthesis (or grouped value) expression.
    /// parenthesis expressions will parse contained expression as a value.
    /// Thus, a parenthesis group is not meant to
    /// see `Parser::statement` for further details.
    fn parenthesis(&mut self) -> ParseResult<Parenthesis<'a>>;
}

impl<'a> GroupAspect<'a> for Parser<'a> {
    fn block(&mut self) -> ParseResult<Block<'a>> {
        self.ensure_at_group_start(TokenType::CurlyLeftBracket, '{')?;
        Ok(Block {
            expressions: self.sub_exprs(TokenType::CurlyRightBracket, Parser::statement)?,
        })
    }

    fn subshell(&mut self) -> ParseResult<Subshell<'a>> {
        self.ensure_at_group_start(TokenType::RoundedLeftBracket, '(')?;
        Ok(Subshell {
            expressions: self.sub_exprs(TokenType::RoundedRightBracket, Parser::statement)?,
        })
    }

    fn parenthesis(&mut self) -> ParseResult<Parenthesis<'a>> {
        self.ensure_at_group_start(TokenType::RoundedLeftBracket, '(')?;
        let expr = self.value()?;
        self.cursor.force(
            repeat(spaces().then(eox())) //consume possible end of expressions
                .then(spaces().then(of_type(TokenType::RoundedRightBracket))), //expect closing ')' token
            "parenthesis in value expression can only contain one expression",
        )?;

        Ok(Parenthesis {
            expression: Box::new(expr),
        })
    }
}

impl<'a> Parser<'a> {
    fn ensure_at_group_start(
        &mut self,
        start: TokenType,
        start_val: char,
    ) -> ParseResult<Token<'a>> {
        self.cursor.force(
            of_type(start),
            &format!(
                "unexpected start of group expression. expected '{}', found '{}'",
                start_val,
                self.cursor.peek().value
            ),
        ) //consume group start token
    }

    ///parses sub expressions of a grouping expression
    fn sub_exprs<F>(&mut self, eog: TokenType, mut parser: F) -> ParseResult<Vec<Expr<'a>>>
    where
        F: FnMut(&mut Self) -> ParseResult<Expr<'a>>,
    {
        let mut statements: Vec<Expr<'a>> = Vec::new();

        //consume all heading spaces and end of expressions (\n or ;)
        self.cursor.advance(repeat(spaces().then(eox())));

        //if we directly hit end of group, return an empty block.
        if self.cursor.advance(of_type(eog)).is_some() {
            return Ok(statements);
        }

        loop {
            let statement = parser(self)?;
            statements.push(statement);

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
        Ok(statements)
    }
}

#[cfg(test)]
mod tests {
    use crate::aspects::group::GroupAspect;
    use crate::ast::callable::Call;
    use crate::ast::group::{Block, Subshell};
    use crate::ast::literal::Literal;
    use crate::ast::literal::LiteralValue::{Float, Int};
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
            Block {
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
            }
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
            Block {
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
            }
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
            (val x = 89; command call; 7)\
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
            Block {
                expressions: vec![
                    Expr::VarDeclaration(VarDeclaration {
                        kind: VarKind::Val,
                        var: TypedVariable {
                            name: "test",
                            ty: None,
                        },
                        initializer: Some(Box::from(Expr::Block(Block {
                            expressions: vec![
                                Expr::VarDeclaration(VarDeclaration {
                                    kind: VarKind::Val,
                                    var: TypedVariable {
                                        name: "x",
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
                    Expr::Subshell(Subshell {
                        expressions: vec![
                            Expr::VarDeclaration(VarDeclaration {
                                kind: VarKind::Val,
                                var: TypedVariable {
                                    name: "x",
                                    ty: None,
                                },
                                initializer: Some(Box::from(Expr::Literal(Literal {
                                    lexme: "89",
                                    parsed: Int(89),
                                }))),
                            }),
                            Expr::Call(Call {
                                arguments: vec![
                                    Expr::Literal("command".into()),
                                    Expr::Literal("call".into()),
                                ],
                            }),
                            Expr::Literal(Literal {
                                lexme: "7",
                                parsed: Int(7),
                            })
                        ]
                    }),
                ]
            }
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
            Block {
                expressions: vec![
                    Expr::VarDeclaration(VarDeclaration {
                        kind: VarKind::Var,
                        var: TypedVariable {
                            name: "test",
                            ty: Some("int"),
                        },
                        initializer: Some(Box::new(Expr::Literal(Literal {
                            lexme: "7.0",
                            parsed: Float(7.0),
                        }))),
                    }),
                    Expr::VarDeclaration(VarDeclaration {
                        kind: VarKind::Val,
                        var: TypedVariable {
                            name: "x",
                            ty: None,
                        },
                        initializer: Some(Box::new(Expr::Literal(Literal {
                            lexme: "8",
                            parsed: Int(8),
                        }))),
                    }),
                ]
            }
        )
    }
}
