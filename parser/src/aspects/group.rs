use ast::group::{Block, Parenthesis, Subshell};
use ast::Expr;
use context::source::SourceSegment;
use lexer::token::{Token, TokenType};

use crate::err::ParseErrorKind;
use crate::moves::{line_end, of_type, of_types, repeat, repeat_n, spaces, Move};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    pub(crate) fn block(&mut self) -> ParseResult<Block> {
        let start = self.ensure_at_group_start(TokenType::CurlyLeftBracket)?;
        let (expressions, segment) =
            self.sub_exprs(start, TokenType::CurlyRightBracket, Parser::declaration)?;
        Ok(Block {
            expressions,
            segment,
        })
    }

    pub(crate) fn subshell(&mut self) -> ParseResult<Subshell> {
        let start = self.ensure_at_group_start(TokenType::RoundedLeftBracket)?;
        let (expressions, segment) = self.sub_exprs(
            start.clone(),
            TokenType::RoundedRightBracket,
            Parser::declaration,
        )?;
        Ok(Subshell {
            expressions,
            segment,
        })
    }

    pub(crate) fn parenthesis(&mut self) -> ParseResult<Parenthesis> {
        let start = self.ensure_at_group_start(TokenType::RoundedLeftBracket)?;
        let expr = self.value().inspect_err(|err| {
            self.repos_delimiter_due_to(err);
        })?;
        self.cursor.advance(spaces());
        if !self.cursor.peek().token_type.is_closing_punctuation() {
            self.expected(
                "parenthesis in value expression can only contain one expression",
                ParseErrorKind::Unexpected,
            )
            .inspect_err(|err| {
                self.repos_delimiter_due_to(err);
            })?
        }
        let end = self.expect_delimiter(start.clone(), TokenType::RoundedRightBracket)?;

        Ok(Parenthesis {
            expression: Box::new(expr),
            segment: start.span.start..end.span.end,
        })
    }

    fn ensure_at_group_start(&mut self, start: TokenType) -> ParseResult<Token> {
        let token = self.cursor.force_with(
            of_type(start),
            "Unexpected start of group expression",
            ParseErrorKind::Expected(start.str().unwrap_or("specific token").to_string()),
        )?; //consume group start token
        Ok(token)
    }

    /// Parses sub expressions of a grouping expression.
    ///
    /// This parser will always end by consuming the closing delimiter of the group,
    /// so it is not necessary to call [`Parser::repos_delimiter_due_to`] on errors.
    fn sub_exprs<F>(
        &mut self,
        start_token: Token,
        eog: TokenType,
        mut parser: F,
    ) -> ParseResult<(Vec<Expr>, SourceSegment)>
    where
        F: FnMut(&mut Self) -> ParseResult<Expr>,
    {
        let mut statements: Vec<Expr> = Vec::new();
        let mut segment = start_token.span.clone();

        //consume all heading spaces and end of expressions (\n or ;)
        self.cursor.advance(repeat(of_types(&[
            TokenType::Space,
            TokenType::NewLine,
            TokenType::SemiColon,
        ])));

        //if we directly hit end of group, return an empty block.
        if let Some(eog) = self.cursor.advance(of_type(eog)) {
            return Ok((statements, segment.start..eog.span.end));
        }

        loop {
            if self.cursor.is_at_end() {
                self.expected(
                    "Expected closing bracket.",
                    ParseErrorKind::Unpaired(start_token.span.clone()),
                )?;
            }
            let statement = parser(self);
            match statement {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(err) => {
                    self.recover_from(err, line_end());
                }
            }

            //expects at least one newline or ';'
            let eox_res = self.cursor.advance(repeat_n(1, spaces().then(line_end())));

            //checks if this group expression is closed after the parsed expression
            let closed = self.cursor.advance(spaces().then(of_type(eog)));

            //if the group is closed, then we stop looking for other expressions.
            if let Some(closing) = closed {
                segment = segment.start..closing.span.end;
                break;
            }

            // Since it is not closed, expect the cursor to hit EOX.
            if eox_res.is_some() {
                continue;
            }
            let error = self.mk_parse_error(
                "expected new line or semicolon",
                self.cursor.peek().span,
                ParseErrorKind::Unexpected,
            );
            self.repos_delimiter_due_to(&error);
            return Err(error);
        }
        Ok((statements, segment))
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::Call;
    use ast::group::{Block, Subshell};
    use ast::r#type::ParametrizedType;
    use ast::r#type::Type;
    use ast::r#use::InclusionPathItem;
    use ast::value::Literal;
    use ast::value::LiteralValue::{Float, Int};
    use ast::variable::{TypedVariable, VarDeclaration, VarKind};
    use ast::Expr;
    use context::source::SourceSegmentHolder;
    use context::str_find::{find_between, find_in, find_in_nth};

    use crate::err::{ParseError, ParseErrorKind, ParseReport};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use crate::source::{identifier, identifier_nth, literal};

    //noinspection DuplicatedCode
    #[test]
    fn empty_blocks() {
        let source = "{{{}; {}}}";
        let mut parser = Parser::new(source);
        let ast = parser.block().expect("failed to parse block");
        assert!(parser.cursor.is_at_end());
        assert_eq!(
            ast,
            Block {
                expressions: vec![Expr::Block(Block {
                    expressions: vec![
                        Expr::Block(Block {
                            expressions: vec![],
                            segment: 2..4
                        }),
                        Expr::Block(Block {
                            expressions: vec![],
                            segment: 6..8
                        }),
                    ],
                    segment: 1..source.len() - 1
                })],
                segment: source.segment()
            }
        );
    }

    //noinspection DuplicatedCode
    #[test]
    fn empty_blocks_empty_source() {
        let source = "{;;{;;;{;;}; {\n\n};}}";
        let mut parser = Parser::new(source);
        let ast = parser.block().expect("failed to parse block");
        assert!(parser.cursor.is_at_end());
        assert_eq!(
            ast,
            Block {
                expressions: vec![Expr::Block(Block {
                    expressions: vec![
                        Expr::Block(Block {
                            expressions: vec![],
                            segment: 7..11
                        }),
                        Expr::Block(Block {
                            expressions: vec![],
                            segment: 13..17
                        }),
                    ],
                    segment: 3..source.len() - 1
                })],
                segment: source.segment()
            }
        );
    }

    #[test]
    fn blank_block() {
        let source = "{ }";
        let result = Parser::new(source).parse_specific(|parser| parser.block());
        assert_eq!(
            result.expect("failed to parse block"),
            Block {
                expressions: vec![],
                segment: source.segment()
            }
        );
    }

    #[test]
    fn block_not_ended() {
        let source = "{ val test = 2 ";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Expected closing bracket.".to_string(),
                position: source.len()..source.len(),
                kind: ParseErrorKind::Unpaired(0..1),
            })
        );
    }

    #[test]
    fn neighbour_parenthesis() {
        let source = "{ {} {} }";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "expected new line or semicolon".to_string(),
                position: find_in_nth(source, "{", 2),
                kind: ParseErrorKind::Unexpected,
            })
        );
    }

    #[test]
    fn block_not_started() {
        let source = " val test = 2 }";
        let mut parser = Parser::new(source);
        parser.block().expect_err("block parse did not failed");
    }

    #[test]
    fn unexpected_token_in_block() {
        let source = "{val x = 5 match}";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "expected new line or semicolon".to_string(),
                position: find_in(source, "match"),
                kind: ParseErrorKind::Unexpected,
            })
        );
    }

    #[test]
    fn block_with_nested_blocks() {
        let source = "{\
            val test = {\
                val x = 8\n\n\n
                8
            }\n
            (val x = 89; command call; 7)\
        }";
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
                            name: identifier(source, "test"),
                            ty: None,
                        },
                        initializer: Some(Box::from(Expr::Block(Block {
                            expressions: vec![
                                Expr::VarDeclaration(VarDeclaration {
                                    kind: VarKind::Val,
                                    var: TypedVariable {
                                        name: identifier(source, "x"),
                                        ty: None,
                                    },
                                    initializer: Some(Box::from(Expr::Literal(Literal {
                                        parsed: Int(8),
                                        segment: find_in(source, "8"),
                                    }))),
                                    segment: find_in(source, "val x = 8"),
                                }),
                                Expr::Literal(Literal {
                                    parsed: Int(8),
                                    segment: find_in_nth(source, "8", 1),
                                }),
                            ],
                            segment: find_between(
                                source,
                                "{\
                val x",
                                "}"
                            ),
                        }))),
                        segment: find_between(source, "val test = {", "}"),
                    }),
                    Expr::Subshell(Subshell {
                        expressions: vec![
                            Expr::VarDeclaration(VarDeclaration {
                                kind: VarKind::Val,
                                var: TypedVariable {
                                    name: identifier_nth(source, "x", 1),
                                    ty: None,
                                },
                                initializer: Some(Box::from(Expr::Literal(Literal {
                                    parsed: Int(89),
                                    segment: find_in(source, "89")
                                }))),
                                segment: find_in(source, "val x = 89"),
                            }),
                            Expr::Call(Call {
                                arguments: vec![
                                    literal(source, "command"),
                                    literal(source, "call")
                                ],
                            }),
                            Expr::Literal(Literal {
                                parsed: Int(7),
                                segment: find_in(source, "7")
                            })
                        ],
                        segment: find_between(source, "(", ")"),
                    }),
                ],
                segment: source.segment()
            }
        )
    }

    #[test]
    fn block() {
        let source = "{\
            var test: int = 7.0\n\
            val x = 8\
        }";
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
                            name: identifier(source, "test"),
                            ty: Some(Type::Parametrized(ParametrizedType {
                                path: vec![InclusionPathItem::Symbol(identifier(source, "int"))],
                                params: Vec::new(),
                                segment: find_in(source, "int")
                            })),
                        },
                        initializer: Some(Box::new(Expr::Literal(Literal {
                            parsed: Float(7.0),
                            segment: find_in(source, "7.0")
                        }))),
                        segment: find_in(source, "var test: int = 7.0"),
                    }),
                    Expr::VarDeclaration(VarDeclaration {
                        kind: VarKind::Val,
                        var: TypedVariable {
                            name: identifier(source, "x"),
                            ty: None,
                        },
                        initializer: Some(Box::new(Expr::Literal(Literal {
                            parsed: Int(8),
                            segment: find_in(source, "8"),
                        }))),
                        segment: find_in(source, "val x = 8"),
                    }),
                ],
                segment: source.segment()
            }
        )
    }

    #[test]
    fn unmatched_closing() {
        let source = "{]}";
        let report = parse(source);
        assert_eq!(
            report,
            ParseReport {
                expr: vec![Expr::Block(Block {
                    expressions: Vec::new(),
                    segment: source.segment(),
                })],
                errors: vec![ParseError {
                    message: "Mismatched closing delimiter.".to_string(),
                    position: source.find(']').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unpaired(source.find('{').map(|p| p..p + 1).unwrap())
                }]
            }
        );
    }

    #[test]
    fn unmatched_opening() {
        let source = "{(}";
        let report = parse(source);
        assert_eq!(
            report,
            ParseReport {
                expr: vec![],
                errors: vec![ParseError {
                    message: "Mismatched closing delimiter.".to_string(),
                    position: source.find('}').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unpaired(source.find('(').map(|p| p..p + 1).unwrap())
                }]
            }
        );
    }
}
