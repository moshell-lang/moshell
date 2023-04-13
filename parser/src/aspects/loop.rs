use lexer::token::{Token, TokenType};

use crate::err::ParseErrorKind;
use crate::moves::{blanks, eod, eox, of_type, times, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::control_flow::{ConditionalFor, For, ForKind, Loop, RangeFor, While};
use ast::range::FilePattern;
use context::source::SourceSegmentHolder;

///a parser aspect for loops and while expressions
pub trait LoopAspect<'a> {
    ///parse a while expression
    fn parse_while(&mut self) -> ParseResult<While<'a>>;
    ///parse a loop expression
    fn parse_loop(&mut self) -> ParseResult<Loop<'a>>;
    /// Parse a for expression
    fn parse_for(&mut self) -> ParseResult<For<'a>>;
}

impl<'a> LoopAspect<'a> for Parser<'a> {
    fn parse_while(&mut self) -> ParseResult<While<'a>> {
        let start = self.cursor.force(
            of_type(TokenType::While),
            "expected 'while' at start of while expression",
        )?;
        //consume blanks before condition
        self.cursor.advance(blanks());
        let condition = Box::new(self.expression_statement()?);

        //consume blanks
        self.cursor.advance(blanks());
        //then consume eox (if any)
        self.cursor.advance(eox());

        let body = Box::new(self.expression_statement()?);
        let segment = self.cursor.relative_pos(start).start..body.segment().end;

        Ok(While {
            condition,
            body,
            segment,
        })
    }

    fn parse_loop(&mut self) -> ParseResult<Loop<'a>> {
        let start = self.cursor.force(
            of_type(TokenType::Loop),
            "expected 'loop' at start of loop expression",
        )?;
        self.cursor.advance(blanks());
        let body = Box::new(self.expression_statement()?);
        let segment = self.cursor.relative_pos(start).start..body.segment().end;

        Ok(Loop { body, segment })
    }

    fn parse_for(&mut self) -> ParseResult<For<'a>> {
        let start = self.cursor.force(
            of_type(TokenType::For),
            "expected 'for' at start of for expression",
        )?;
        self.cursor.advance(blanks());
        let kind = Box::new(self.parse_for_kind()?);
        self.cursor.advance(eox());
        let body = Box::new(self.expression_statement()?);
        let segment = self.cursor.relative_pos(start).start..body.segment().end;

        Ok(For {
            kind,
            body,
            segment,
        })
    }
}

impl<'a> Parser<'a> {
    /// Parses the for kind, either a range for or a conditional for.
    fn parse_for_kind(&mut self) -> ParseResult<ForKind<'a>> {
        let current = self.cursor.peek();
        let start_pos = self.cursor.relative_pos(&current).start;
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
                    let end_pos = self.cursor.relative_pos(self.cursor.peek()).end;
                    let slice = &self.source.source[start_pos + 1..end_pos];
                    return self.expected_with(
                        "Receiver variables do not start with '$'.",
                        current,
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
    fn parse_range_for(&mut self) -> ParseResult<RangeFor<'a>> {
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
        let segment = self.cursor.relative_pos(&receiver).start..iterable.segment().end;

        Ok(RangeFor {
            receiver: receiver.value,
            iterable,
            segment,
        })
    }

    /// Parses a "traditional" conditional for, with a initializer, a condition and an increment.
    fn parse_conditional_for(&mut self) -> ParseResult<ConditionalFor<'a>> {
        let start = self
            .cursor
            .collect(times(2, of_type(TokenType::RoundedLeftBracket)));
        if start.is_empty() {
            return self.expected(
                "Expected '((' at start of conditional for",
                ParseErrorKind::Unexpected,
            );
        }
        self.delimiter_stack.extend(start.clone());

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

        let mut end: Option<Token<'a>> = None;
        for _ in 0..2 {
            if self.cursor.lookahead(eod()).is_some() {
                end = Some(self.expect_delimiter(TokenType::RoundedRightBracket)?);
            } else {
                self.expected(
                    "Expected '))' at end of conditional for",
                    ParseErrorKind::Unpaired(self.cursor.relative_pos_ctx(&start[..])),
                )?;
            }
        }

        let segment = self
            .cursor
            .relative_pos_ctx(start.first().unwrap().clone()..end.unwrap());

        Ok(ConditionalFor {
            initializer,
            condition,
            increment,
            segment,
        })
    }

    /// Parse a file pattern.
    ///
    /// For now, this is just a single wildcard star.
    fn parse_files_pattern(&mut self) -> ParseResult<FilePattern> {
        let lexme = self
            .cursor
            .force(of_type(TokenType::Star), "expected '*'")?;
        Ok(FilePattern {
            pattern: lexme.value.to_owned(),
            segment: self.cursor.relative_pos_ctx(lexme),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::err::ParseError;
    use crate::err::ParseErrorKind::Unexpected;
    use crate::parse;
    use crate::parser::ParseResult;

    use crate::source::literal;
    use ast::call::Call;
    use ast::control_flow::{ConditionalFor, For, ForKind, Loop, RangeFor, While};
    use ast::group::{Block, Parenthesis};
    use ast::operation::BinaryOperator::And;
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::range::{FilePattern, Iterable, NumericRange};
    use ast::value::Literal;
    use ast::variable::{Assign, TypedVariable, VarDeclaration, VarKind, VarReference};
    use ast::Expr;
    use ast::Expr::{Break, Continue};
    use context::source::{Source, SourceSegmentHolder};
    use context::str_find::{find_between, find_in, find_in_nth};
    use pretty_assertions::assert_eq;

    #[test]
    fn loop_with_break_and_continues() {
        let source = Source::unknown(
            "loop {
            continue; break;
            }",
        );
        let res = parse(source.clone()).expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::Loop(Loop {
                body: Box::new(Expr::Block(Block {
                    expressions: vec![
                        Continue(find_in(source.source, "continue")),
                        Break(find_in(source.source, "break"))
                    ],
                    segment: find_between(source.source, "{", "}")
                })),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn loop_with_break_and_continues_inline() {
        let source = Source::unknown("loop ssh mabatista1@iut && break");
        let res = parse(source.clone()).expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::Loop(Loop {
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Call(Call {
                        path: Vec::new(),
                        arguments: vec![
                            Expr::Literal(Literal {
                                parsed: "ssh".into(),
                                segment: find_in(source.source, "ssh"),
                            }),
                            Expr::Literal(Literal {
                                parsed: "mabatista1@iut".into(),
                                segment: find_in(source.source, "mabatista1@iut"),
                            }),
                        ],
                        type_parameters: vec![],
                    })),
                    op: And,
                    right: Box::new(Break(find_in(source.source, "break"))),
                })),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn test_loop() {
        let source = Source::unknown("loop \n\n \n \n date");
        let res = parse(source.clone()).expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::Loop(Loop {
                body: Box::new(Expr::Call(Call {
                    path: Vec::new(),
                    arguments: vec![Expr::Literal(Literal {
                        parsed: "date".into(),
                        segment: find_in(source.source, "date"),
                    })],
                    type_parameters: vec![],
                })),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn loop_no_body() {
        let content = "loop";
        let res: ParseResult<_> = parse(Source::unknown(content)).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "Expected expression statement".to_string(),
                position: content.len()..content.len(),
                kind: Unexpected,
            })
        )
    }

    #[test]
    fn test_while() {
        let source = Source::unknown("while \n\n \n \n $1 \n\n \n{ echo test }");
        let res = parse(source.clone()).expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::While(While {
                condition: Box::new(Expr::VarReference(VarReference {
                    name: "1",
                    segment: find_in(source.source, "$1"),
                })),
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        path: Vec::new(),
                        arguments: vec![
                            literal(source.source, "echo"),
                            literal(source.source, "test"),
                        ],
                        type_parameters: vec![],
                    })],
                    segment: find_in(source.source, "{ echo test }")
                })),
                segment: source.segment()
            })]
        )
    }

    #[test]
    fn for_in_int_range() {
        let source = Source::unknown("for i in 1..10 {\n\techo $i\n}");
        let expr = parse(source.clone()).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Range(RangeFor {
                    receiver: "i",
                    iterable: Expr::Range(Iterable::Range(NumericRange {
                        start: Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in(source.source, "1")
                        })),
                        end: Box::new(Expr::Literal(Literal {
                            parsed: 10.into(),
                            segment: find_in(source.source, "10")
                        })),
                        step: None,
                        upper_inclusive: false,
                    })),
                    segment: find_in(source.source, "i in 1..10"),
                })),
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        path: Vec::new(),
                        arguments: vec![
                            Expr::Literal(Literal {
                                parsed: "echo".into(),
                                segment: find_in(source.source, "echo")
                            }),
                            Expr::VarReference(VarReference {
                                name: "i",
                                segment: find_in(source.source, "$i")
                            }),
                        ],
                        type_parameters: vec![],
                    })],
                    segment: find_in(source.source, "{\n\techo $i\n}")
                })),
                segment: source.segment()
            })]
        );
    }

    #[test]
    fn for_in_variable_range() {
        let source = Source::unknown("for n in $a..$b; cat");
        let expr = parse(source.clone()).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Range(RangeFor {
                    receiver: "n",
                    iterable: Expr::Range(Iterable::Range(NumericRange {
                        start: Box::new(Expr::VarReference(VarReference {
                            name: "a",
                            segment: find_in(source.source, "$a")
                        })),
                        end: Box::new(Expr::VarReference(VarReference {
                            name: "b",
                            segment: find_in(source.source, "$b")
                        })),
                        step: None,
                        upper_inclusive: false,
                    })),
                    segment: find_in(source.source, "n in $a..$b"),
                })),
                body: Box::new(Expr::Call(Call {
                    path: Vec::new(),
                    arguments: vec![Expr::Literal(Literal {
                        parsed: "cat".into(),
                        segment: find_in(source.source, "cat")
                    })],
                    type_parameters: vec![],
                })),
                segment: source.segment(),
            })]
        );
    }

    #[test]
    fn for_in_calculated_range() {
        let source = Source::unknown("for i in (1 + 2)..5; ls");
        let expr = parse(source.clone()).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Range(RangeFor {
                    receiver: "i",
                    iterable: Expr::Range(Iterable::Range(NumericRange {
                        start: Box::new(Expr::Parenthesis(Parenthesis {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    parsed: 1.into(),
                                    segment: find_in(source.source, "1")
                                })),
                                op: BinaryOperator::Plus,
                                right: Box::new(Expr::Literal(Literal {
                                    parsed: 2.into(),
                                    segment: find_in(source.source, "2")
                                })),
                            })),
                            segment: find_in(source.source, "(1 + 2)")
                        })),
                        end: Box::new(Expr::Literal(Literal {
                            parsed: 5.into(),
                            segment: find_in(source.source, "5")
                        })),
                        step: None,
                        upper_inclusive: false,
                    })),
                    segment: find_in(source.source, "i in (1 + 2)..5"),
                })),
                body: Box::new(Expr::Call(Call {
                    path: Vec::new(),
                    arguments: vec![Expr::Literal(Literal {
                        parsed: "ls".into(),
                        segment: find_in(source.source, "ls")
                    })],
                    type_parameters: vec![],
                })),
                segment: source.segment()
            })]
        );
    }

    #[test]
    fn for_in_files_range() {
        let source = Source::unknown("for f in * {\n\tfile $f\n}");
        let expr = parse(source.clone()).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Range(RangeFor {
                    receiver: "f",
                    iterable: Expr::Range(Iterable::Files(FilePattern {
                        pattern: "*".to_owned(),
                        segment: find_in(source.source, "*")
                    })),
                    segment: find_in(source.source, "f in *"),
                })),
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        path: Vec::new(),
                        arguments: vec![
                            Expr::Literal(Literal {
                                parsed: "file".into(),
                                segment: find_in(source.source, "file")
                            }),
                            Expr::VarReference(VarReference {
                                name: "f",
                                segment: find_in(source.source, "$f")
                            }),
                        ],
                        type_parameters: vec![],
                    })],
                    segment: find_in(source.source, "{\n\tfile $f\n}")
                })),
                segment: source.segment(),
            })]
        );
    }

    #[test]
    fn classical_for() {
        let source = Source::unknown("for (( var i=0; $i<10; i=$i + 1 ))\necho $i");
        let expr = parse(source.clone()).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Conditional(ConditionalFor {
                    initializer: Expr::VarDeclaration(VarDeclaration {
                        kind: VarKind::Var,
                        var: TypedVariable {
                            name: "i",
                            ty: None,
                            segment: find_in(source.source, "i")
                        },
                        initializer: Some(Box::new(Expr::Literal(Literal {
                            parsed: 0.into(),
                            segment: find_in(source.source, "0")
                        }))),
                        segment: find_in(source.source, "var i=0")
                    }),
                    condition: Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::VarReference(VarReference {
                            name: "i",
                            segment: find_in(source.source, "$i")
                        })),
                        op: BinaryOperator::Less,
                        right: Box::new(Expr::Literal(Literal {
                            parsed: 10.into(),
                            segment: find_in(source.source, "10")
                        })),
                    }),
                    increment: Expr::Assign(Assign {
                        name: "i",
                        value: Box::new(Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::VarReference(VarReference {
                                name: "i",
                                segment: find_in_nth(source.source, "$i", 1)
                            })),
                            op: BinaryOperator::Plus,
                            right: Box::new(Expr::Literal(Literal {
                                parsed: 1.into(),
                                segment: find_in_nth(source.source, "1", 1)
                            })),
                        })),
                        segment: find_in(source.source, "i=$i + 1")
                    }),
                    segment: find_between(source.source, "((", "))")
                })),
                body: Box::new(Expr::Call(Call {
                    path: Vec::new(),
                    arguments: vec![
                        literal(source.source, "echo"),
                        Expr::VarReference(VarReference {
                            name: "i",
                            segment: find_in_nth(source.source, "$i", 2)
                        }),
                    ],
                    type_parameters: vec![],
                })),
                segment: source.segment(),
            })]
        );
    }

    #[test]
    fn for_into_nothing() {
        let content = "for in 1..5";
        let source = Source::unknown(content);
        let expr: ParseResult<_> = parse(source).into();
        assert_eq!(
            expr,
            Err(ParseError {
                message: "Expected variable name before 'in'".to_string(),
                position: content.find("in").map(|p| p..p + 2).unwrap(),
                kind: Unexpected,
            })
        );
    }

    #[test]
    fn while_no_condition() {
        let content = "while";
        let res: ParseResult<_> = parse(Source::unknown(content)).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "Expected expression statement".to_string(),
                position: content.len()..content.len(),
                kind: Unexpected,
            })
        )
    }

    #[test]
    fn while_no_body() {
        let content = "while $x";
        let res: ParseResult<_> = parse(Source::unknown(content)).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "Expected expression statement".to_string(),
                position: content.len()..content.len(),
                kind: Unexpected,
            })
        )
    }
}
