use ast::control_flow::{ConditionalFor, For, ForKind, Loop, RangeFor, While};
use ast::variable::Identifier;
use context::source::SourceSegmentHolder;
use lexer::token::{Token, TokenType};

use crate::err::ParseErrorKind;
use crate::moves::{blanks, eog, line_end, of_type, MoveOperations};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    /// Parses a `while` expression.
    pub(crate) fn parse_while(&mut self) -> ParseResult<While> {
        let start = self.cursor.force(
            of_type(TokenType::While),
            "expected 'while' at start of while expression",
        )?;
        //consume blanks before condition
        self.cursor.advance(blanks());
        let condition = Box::new(self.statement()?);

        //consume blanks
        self.cursor.advance(blanks());
        //then consume eox (if any)
        self.cursor.advance(line_end());

        let body = Box::new(self.statement()?);
        let segment = start.span.start..body.segment().end;

        Ok(While {
            condition,
            body,
            segment,
        })
    }

    /// Parses a `loop` expression.
    pub(crate) fn parse_loop(&mut self) -> ParseResult<Loop> {
        let start = self.cursor.force(
            of_type(TokenType::Loop),
            "expected 'loop' at start of loop expression",
        )?;
        self.cursor.advance(blanks());
        let body = Box::new(self.statement()?);
        let segment = start.span.start..body.segment().end;

        Ok(Loop { body, segment })
    }

    /// Parses a `for` expression.
    pub(crate) fn parse_for(&mut self) -> ParseResult<For> {
        let start = self.cursor.force(
            of_type(TokenType::For),
            "expected 'for' at start of for expression",
        )?;
        self.cursor.advance(blanks());
        let kind = Box::new(self.parse_for_kind()?);
        self.cursor.advance(line_end());
        let body = Box::new(self.statement()?);
        let segment = start.span.start..body.segment().end;

        Ok(For {
            kind,
            body,
            segment,
        })
    }

    /// Parses the for kind, either a range for or a conditional for.
    fn parse_for_kind(&mut self) -> ParseResult<ForKind> {
        let current = self.cursor.peek();
        let start_pos = current.span.start;
        match current.token_type {
            TokenType::Identifier => {
                let range_for = self.parse_range_for()?;
                Ok(ForKind::Range(range_for))
            }
            TokenType::RoundedLeftBracket => {
                let conditional_for = self.parse_conditional_for()?;
                Ok(ForKind::Conditional(conditional_for))
            }

            // Those tokens are not valid here, but we can try to parse them and give a better error
            // message.
            TokenType::Dollar => {
                self.cursor.next_opt();
                if self.parse_range_for().is_ok() {
                    let end_pos = self.cursor.peek().span.end;
                    let slice = &self.source[start_pos + 1..end_pos];
                    return self.expected_with(
                        "Receiver variables do not start with '$'.",
                        current.span,
                        ParseErrorKind::UnexpectedInContext(format!(
                            "Consider removing the '$' prefix: for {slice}"
                        )),
                    );
                }
                self.expected("Expected identifier", ParseErrorKind::Unexpected)
            }
            TokenType::In => self.expected(
                "Expected variable name before 'in'",
                ParseErrorKind::Unexpected,
            ),
            _ => self.expected("Expected identifier or '(('", ParseErrorKind::Unexpected),
        }
    }

    /// Parses a for loop with range, with a receiver and an iterable.
    fn parse_range_for(&mut self) -> ParseResult<RangeFor> {
        let receiver = self.cursor.force(
            of_type(TokenType::Identifier),
            "Expected a variable identifier",
        )?;
        self.cursor.advance(blanks());
        self.cursor.force(
            of_type(TokenType::In),
            "expected 'in' after receiver in range for",
        )?;
        self.cursor.advance(blanks());
        let iterable = self.value()?;
        let segment = receiver.span.start..iterable.segment().end;

        Ok(RangeFor {
            receiver: Identifier::extract(self.source, receiver.span),
            iterable,
            segment,
        })
    }

    /// Parses a "traditional" conditional for, with a initializer, a condition and an increment.
    fn parse_conditional_for(&mut self) -> ParseResult<ConditionalFor> {
        let outer_opening_parenthesis = self.cursor.force(
            of_type(TokenType::RoundedLeftBracket),
            "expected '((' at start of conditional for",
        )?;
        let kind = self.parse_inner_conditional_for(outer_opening_parenthesis.clone());

        match kind {
            Ok(mut kind) => {
                let end =
                    self.expect_one_closing_parentheses_in_for(outer_opening_parenthesis.clone())?;
                kind.segment = outer_opening_parenthesis.span.start..end.span.end;
                Ok(kind)
            }
            Err(err) => {
                self.repos_delimiter_due_to(&err);
                Err(err)
            }
        }
    }
    fn parse_inner_conditional_for(&mut self, start: Token) -> ParseResult<ConditionalFor> {
        self.cursor.force(
            of_type(TokenType::RoundedLeftBracket),
            "expected '((' at start of conditional for",
        )?;
        match self.parse_three_parts_for() {
            Ok(kind) => {
                self.expect_one_closing_parentheses_in_for(start)?;
                Ok(kind)
            }
            Err(err) => {
                // Errors always recover from only one delimiter, so we force also removing the inner delimiter.
                self.repos_to_top_delimiter();
                Err(err)
            }
        }
    }

    fn parse_three_parts_for(&mut self) -> ParseResult<ConditionalFor> {
        let initializer = self.statement()?;
        self.cursor.force(
            blanks().then(of_type(TokenType::SemiColon)),
            "expected ';' after initializer in conditional for",
        )?;
        let condition = self.value()?;
        self.cursor.force(
            blanks().then(of_type(TokenType::SemiColon)),
            "expected ';' after condition in conditional for",
        )?;
        let increment = self.statement()?;
        let segment = initializer.segment().start..increment.segment().end;
        Ok(ConditionalFor {
            initializer,
            condition,
            increment,
            segment,
        })
    }

    fn expect_one_closing_parentheses_in_for(
        &mut self,
        outer_opening_parenthesis: Token,
    ) -> ParseResult<Token> {
        if self.cursor.lookahead(eog()).is_some() {
            self.expect_delimiter(outer_opening_parenthesis, TokenType::RoundedRightBracket)
        } else {
            let mut segment = outer_opening_parenthesis.span;
            segment.end += 1;
            self.expected(
                "Expected '))' at end of conditional for",
                ParseErrorKind::Unpaired(segment),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::Call;
    use ast::control_flow::{ConditionalFor, For, ForKind, Loop, RangeFor, While};
    use ast::group::{Block, Parenthesis};
    use ast::operation::BinaryOperator::And;
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::r#use::InclusionPathItem;
    use ast::range::{FilePattern, Iterable, NumericRange};
    use ast::value::Literal;
    use ast::variable::{
        Assign, AssignOperator, Path, TypedVariable, VarDeclaration, VarKind, VarName, VarReference,
    };
    use ast::Expr;
    use ast::Expr::{Break, Continue};
    use context::source::SourceSegmentHolder;
    use context::str_find::{find_between, find_in, find_in_nth};

    use crate::err::ParseError;
    use crate::err::ParseErrorKind::Unexpected;
    use crate::parse;
    use crate::parser::ParseResult;
    use crate::source::{identifier, identifier_nth, literal};

    #[test]
    fn loop_with_break_and_continues() {
        let source = "loop {
            continue; break;
            }";
        let res = parse(source).expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::Loop(Loop {
                body: Box::new(Expr::Block(Block {
                    expressions: vec![
                        Continue(find_in(source, "continue")),
                        Break(find_in(source, "break"))
                    ],
                    segment: find_between(source, "{", "}")
                })),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn loop_with_break_and_continues_inline() {
        let source = "loop ssh mabatista1@iut && break";
        let res = parse(source).expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::Loop(Loop {
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal(Literal {
                                parsed: "ssh".into(),
                                segment: find_in(source, "ssh"),
                            }),
                            Expr::Literal(Literal {
                                parsed: "mabatista1@iut".into(),
                                segment: find_in(source, "mabatista1@iut"),
                            }),
                        ],
                    })),
                    op: And,
                    right: Box::new(Break(find_in(source, "break"))),
                })),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn test_loop() {
        let source = "loop \n\n \n \n date";
        let res = parse(source).expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::Loop(Loop {
                body: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal(Literal {
                        parsed: "date".into(),
                        segment: find_in(source, "date"),
                    })],
                })),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn loop_no_body() {
        let source = "loop";
        let res: ParseResult<_> = parse(source).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "Expected statement".to_string(),
                position: source.len()..source.len(),
                kind: Unexpected,
            })
        )
    }

    #[test]
    fn test_while() {
        let source = "while \n\n \n \n $1 \n\n \n{ echo test }";
        let res = parse(source).expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::While(While {
                condition: Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("1".into()),
                    segment: find_in(source, "$1"),
                })),
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![literal(source, "echo"), literal(source, "test"),],
                    })],
                    segment: find_in(source, "{ echo test }")
                })),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn for_in_int_range() {
        let source = "for i in 1..10 {\n\techo $i\n}";
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Range(RangeFor {
                    receiver: identifier(source, "i"),
                    iterable: Expr::Range(Iterable::Range(NumericRange {
                        start: Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in(source, "1")
                        })),
                        end: Box::new(Expr::Literal(Literal {
                            parsed: 10.into(),
                            segment: find_in(source, "10")
                        })),
                        step: None,
                        upper_inclusive: false,
                    })),
                    segment: find_in(source, "i in 1..10"),
                })),
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal(Literal {
                                parsed: "echo".into(),
                                segment: find_in(source, "echo")
                            }),
                            Expr::VarReference(VarReference {
                                name: VarName::User("i".into()),
                                segment: find_in(source, "$i")
                            }),
                        ],
                    })],
                    segment: find_in(source, "{\n\techo $i\n}")
                })),
                segment: source.segment()
            })]
        );
    }

    #[test]
    fn for_in_variable_range() {
        let source = "for n in $a..$b; cat";
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Range(RangeFor {
                    receiver: identifier(source, "n"),
                    iterable: Expr::Range(Iterable::Range(NumericRange {
                        start: Box::new(Expr::VarReference(VarReference {
                            name: VarName::User("a".into()),
                            segment: find_in(source, "$a")
                        })),
                        end: Box::new(Expr::VarReference(VarReference {
                            name: VarName::User("b".into()),
                            segment: find_in(source, "$b")
                        })),
                        step: None,
                        upper_inclusive: false,
                    })),
                    segment: find_in(source, "n in $a..$b"),
                })),
                body: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal(Literal {
                        parsed: "cat".into(),
                        segment: find_in(source, "cat")
                    })],
                })),
                segment: source.segment(),
            })]
        );
    }

    #[test]
    fn for_in_calculated_range() {
        let source = "for i in (1 + 2)..5; ls";
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Range(RangeFor {
                    receiver: identifier(source, "i"),
                    iterable: Expr::Range(Iterable::Range(NumericRange {
                        start: Box::new(Expr::Parenthesis(Parenthesis {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    parsed: 1.into(),
                                    segment: find_in(source, "1")
                                })),
                                op: BinaryOperator::Plus,
                                right: Box::new(Expr::Literal(Literal {
                                    parsed: 2.into(),
                                    segment: find_in(source, "2")
                                })),
                            })),
                            segment: find_in(source, "(1 + 2)")
                        })),
                        end: Box::new(Expr::Literal(Literal {
                            parsed: 5.into(),
                            segment: find_in(source, "5")
                        })),
                        step: None,
                        upper_inclusive: false,
                    })),
                    segment: find_in(source, "i in (1 + 2)..5"),
                })),
                body: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal(Literal {
                        parsed: "ls".into(),
                        segment: find_in(source, "ls")
                    })],
                })),
                segment: source.segment()
            })]
        );
    }

    #[test]
    fn for_in_files_range() {
        let source = "for f in p'*' {\n\tfile $f\n}";
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Range(RangeFor {
                    receiver: identifier_nth(source, "f", 1),
                    iterable: Expr::Range(Iterable::Files(FilePattern {
                        pattern: Box::new(literal(source, "'*'")),
                        segment: find_in(source, "p'*'")
                    })),
                    segment: find_in(source, "f in p'*'"),
                })),
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal(Literal {
                                parsed: "file".into(),
                                segment: find_in(source, "file")
                            }),
                            Expr::VarReference(VarReference {
                                name: VarName::User("f".into()),
                                segment: find_in(source, "$f")
                            }),
                        ],
                    })],
                    segment: find_in(source, "{\n\tfile $f\n}")
                })),
                segment: source.segment(),
            })]
        );
    }

    #[test]
    fn classical_for() {
        let source = "for (( var i=0; $i<10; i=$i + 1 ))\necho $i";
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Conditional(ConditionalFor {
                    initializer: Expr::VarDeclaration(VarDeclaration {
                        kind: VarKind::Var,
                        var: TypedVariable {
                            name: identifier(source, "i"),
                            ty: None,
                        },
                        initializer: Some(Box::new(Expr::Literal(Literal {
                            parsed: 0.into(),
                            segment: find_in(source, "0")
                        }))),
                        segment: find_in(source, "var i=0")
                    }),
                    condition: Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::VarReference(VarReference {
                            name: VarName::User("i".into()),
                            segment: find_in(source, "$i")
                        })),
                        op: BinaryOperator::Less,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 10.into(),
                            segment: find_in(source, "10")
                        })),
                    }),
                    increment: Expr::Assign(Assign {
                        left: Box::new(Expr::Path(Path {
                            path: vec![InclusionPathItem::Symbol(identifier_nth(source, "i", 2))],
                        })),
                        operator: AssignOperator::Assign,
                        value: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::VarReference(VarReference {
                                name: VarName::User("i".into()),
                                segment: find_in_nth(source, "$i", 1)
                            })),
                            op: BinaryOperator::Plus,
                            right: Box::new(Expr::Literal(Literal {
                                parsed: 1.into(),
                                segment: find_in_nth(source, "1", 1)
                            })),
                        })),
                    }),
                    segment: find_between(source, "((", "))")
                })),
                body: Box::new(Expr::Call(Call {
                    arguments: vec![
                        literal(source, "echo"),
                        Expr::VarReference(VarReference {
                            name: VarName::User("i".into()),
                            segment: find_in_nth(source, "$i", 2)
                        }),
                    ],
                })),
                segment: source.segment(),
            })]
        );
    }

    #[test]
    fn for_into_nothing() {
        let source = "for in 1..5";
        let expr: ParseResult<_> = parse(source).into();
        assert_eq!(
            expr,
            Err(ParseError {
                message: "Expected variable name before 'in'".to_string(),
                position: source.find("in").map(|p| p..p + 2).unwrap(),
                kind: Unexpected,
            })
        );
    }

    #[test]
    fn while_no_condition() {
        let source = "while";
        let res: ParseResult<_> = parse(source).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "Expected statement".to_string(),
                position: source.len()..source.len(),
                kind: Unexpected,
            })
        )
    }

    #[test]
    fn while_no_body() {
        let source = "while $x";
        let res: ParseResult<_> = parse(source).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "Expected statement".to_string(),
                position: source.len()..source.len(),
                kind: Unexpected,
            })
        )
    }
}
