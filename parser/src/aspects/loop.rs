use crate::aspects::var_reference::VarReferenceAspect;
use lexer::token::TokenType;

use crate::ast::control_flow::{ConditionalFor, For, ForKind, Loop, RangeFor, While};
use crate::ast::range::{FilePattern, Iterable, NumericRange};
use crate::ast::value::LiteralValue;
use crate::ast::Expr;
use crate::err::ParseErrorKind;
use crate::moves::{blanks, eod, eox, next, of_type, repeat_nm, MoveOperations};
use crate::parser::{ParseResult, Parser};

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
        self.cursor.force(
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

        Ok(While { condition, body })
    }

    fn parse_loop(&mut self) -> ParseResult<Loop<'a>> {
        self.cursor.force(
            of_type(TokenType::Loop),
            "expected 'loop' at start of loop expression",
        )?;
        self.cursor.advance(blanks());
        let body = Box::new(self.expression_statement()?);

        Ok(Loop { body })
    }

    fn parse_for(&mut self) -> ParseResult<For<'a>> {
        self.cursor.force(
            of_type(TokenType::For),
            "expected 'for' at start of for expression",
        )?;
        self.cursor.advance(blanks());
        let kind = Box::new(self.parse_for_kind()?);
        self.cursor.advance(eox());
        let body = Box::new(self.expression_statement()?);

        Ok(For { kind, body })
    }
}

impl<'a> Parser<'a> {
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
            TokenType::Dollar => {
                self.cursor.next_opt();
                if self.parse_range_for().is_ok() {
                    let end_pos = self.cursor.relative_pos(&self.cursor.peek()).end;
                    let slice = &self.source.source[start_pos + 1..end_pos];
                    return self.expected_with(
                        "Receiver variables do not start with '$'.",
                        current,
                        ParseErrorKind::UnexpectedInContext(format!(
                            "Consider removing the '$' prefix: for {slice}"
                        )),
                    );
                }
                self.expected("Excepted identifier", ParseErrorKind::Unexpected)
            }
            _ => self.expected("Excepted identifier or '['", ParseErrorKind::Unexpected),
        }
    }

    fn parse_range_for(&mut self) -> ParseResult<RangeFor<'a>> {
        let receiver = self.cursor.force(
            of_type(TokenType::Identifier),
            "Excepted a variable identifier",
        )?;
        self.cursor.advance(blanks());
        self.cursor.force(
            of_type(TokenType::In),
            "expected 'in' after receiver in range for",
        )?;
        self.cursor.advance(blanks());
        let iterable = self.parse_iterable()?;

        Ok(RangeFor {
            receiver: receiver.value,
            iterable,
        })
    }

    fn parse_conditional_for(&mut self) -> ParseResult<ConditionalFor<'a>> {
        let start = self.cursor.force(
            repeat_nm(2, 2, of_type(TokenType::RoundedLeftBracket)),
            "expected '((' at start of conditional for",
        )?;
        for _ in 0..2 {
            self.delimiter_stack.push_back(start.clone());
        }

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

        if self.cursor.lookahead(eod()).is_some() {
            for _ in 0..2 {
                self.expect_delimiter(TokenType::RoundedRightBracket)?;
            }
        } else {
            self.expected(
                "Expected '))' at end of conditional for",
                ParseErrorKind::Unexpected,
            )?;
        }

        Ok(ConditionalFor {
            initializer,
            condition,
            increment,
        })
    }

    fn parse_iterable(&mut self) -> ParseResult<Iterable<'a>> {
        let current = self.cursor.peek();
        match current.token_type {
            TokenType::Dollar | TokenType::IntLiteral | TokenType::RoundedLeftBracket => {
                self.parse_range()
            }
            TokenType::Star => self.parse_files_pattern().map(Iterable::Files),
            _ => Err(self.cursor.mk_parse_error(
                "Expected iterable",
                current,
                ParseErrorKind::Unexpected,
            )),
        }
    }

    fn parse_range(&mut self) -> ParseResult<Iterable<'a>> {
        let start_token = self.cursor.peek();
        let start = if start_token.token_type == TokenType::Dollar {
            self.cursor.advance(next());
            self.var_reference()?
        } else {
            self.next_value()?
        };
        if self
            .cursor
            .advance(repeat_nm(2, 2, of_type(TokenType::Dot)))
            .is_none()
        {
            if let Expr::VarReference(path) = start {
                return Ok(Iterable::Var(path));
            }
            return self.expected_with(
                "Expected a numeric range, got a single value.",
                start_token..self.cursor.peek(),
                Self::suggest_range(&start),
            );
        }
        let end = self.next_value()?;
        let mut step: Option<Expr<'a>> = None;
        if self.cursor.advance(of_type(TokenType::Dot)).is_some() {
            self.cursor.force(
                of_type(TokenType::Dot),
                "expected '..' before step of range",
            )?;
            step = Some(self.next_value()?);
        }
        Ok(Iterable::Range(NumericRange {
            start,
            end,
            step,
            upper_inclusive: false,
        }))
    }

    fn parse_files_pattern(&mut self) -> ParseResult<FilePattern<'a>> {
        let lexme = self
            .cursor
            .force(of_type(TokenType::Star), "expected '*'")?;
        Ok(FilePattern {
            lexeme: lexme.value,
            pattern: lexme.value.to_owned(),
        })
    }

    fn suggest_range(actual_expr: &Expr) -> ParseErrorKind {
        if let Expr::Literal(literal) = actual_expr {
            if let LiteralValue::Int(value) = literal.parsed {
                ParseErrorKind::UnexpectedInContext(if value > 0 {
                    format!("Use the range syntax: 0..{value}")
                } else {
                    format!("Use the range syntax: 0..10")
                })
            } else {
                ParseErrorKind::Unexpected
            }
        } else {
            ParseErrorKind::Unexpected
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::callable::Call;
    use crate::ast::control_flow::{ConditionalFor, For, ForKind, Loop, RangeFor, While};
    use crate::ast::group::{Block, Parenthesis};
    use crate::ast::operation::BinaryOperator::And;
    use crate::ast::operation::{BinaryOperation, BinaryOperator};
    use crate::ast::range::{FilePattern, Iterable, NumericRange};
    use crate::ast::value::Literal;
    use crate::ast::variable::{Assign, TypedVariable, VarDeclaration, VarKind, VarReference};
    use crate::ast::Expr;
    use crate::ast::Expr::{Break, Continue};
    use crate::err::ParseErrorKind::Unexpected;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::ParseResult;
    use context::source::Source;
    use pretty_assertions::assert_eq;

    #[test]
    fn loop_with_break_and_continues() {
        let res = parse(Source::unknown(
            "loop {
            continue; break;
            }",
        ))
        .expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::Loop(Loop {
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Continue, Break]
                }))
            })]
        )
    }

    #[test]
    fn loop_with_break_and_continues_inline() {
        let res = parse(Source::unknown("loop ssh mabatista1@iut && break")).expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::Loop(Loop {
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal("ssh".into()),
                            Expr::Literal("mabatista1@iut".into())
                        ]
                    })),
                    op: And,
                    right: Box::new(Break)
                }))
            })]
        )
    }

    #[test]
    fn test_loop() {
        let res = parse(Source::unknown("loop \n\n \n \n date")).expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::Loop(Loop {
                body: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("date".into())]
                }))
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
        let res = parse(Source::unknown("while \n\n \n \n $1 \n\n \n{ echo test }"))
            .expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::While(While {
                condition: Box::new(Expr::VarReference(VarReference { name: "1" })),
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![Expr::Literal("echo".into()), Expr::Literal("test".into())]
                    })]
                })),
            })]
        )
    }

    #[test]
    fn for_in_int_range() {
        let source = Source::unknown("for i in 1..10 {\n\techo $i\n}");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Range(RangeFor {
                    receiver: "i",
                    iterable: Iterable::Range(NumericRange {
                        start: Expr::Literal(Literal {
                            lexeme: "1",
                            parsed: 1.into(),
                        }),
                        end: Expr::Literal(Literal {
                            lexeme: "10",
                            parsed: 10.into(),
                        }),
                        step: None,
                        upper_inclusive: false,
                    })
                })),
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal("echo".into()),
                            Expr::VarReference(VarReference { name: "i" })
                        ]
                    })]
                }))
            })]
        );
    }

    #[test]
    fn for_in_variable_range() {
        let source = Source::unknown("for n in $a..$b; cat"); // value parser is too greedy
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Range(RangeFor {
                    receiver: "n",
                    iterable: Iterable::Range(NumericRange {
                        start: Expr::VarReference(VarReference { name: "a" }),
                        end: Expr::VarReference(VarReference { name: "b" }),
                        step: None,
                        upper_inclusive: false,
                    })
                })),
                body: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("cat".into())]
                }))
            })]
        );
    }

    #[test]
    fn for_in_invalid_range() {
        let content = "for i in 5; ls";
        let source = Source::unknown(content);
        let expr: ParseResult<_> = parse(source).into();
        assert_eq!(
            expr,
            Err(ParseError {
                message: "Expected a numeric range, got a single value.".to_string(),
                position: content.find('5').map(|p| p..p + 2).unwrap(),
                kind: ParseErrorKind::UnexpectedInContext("Use the range syntax: 0..5".to_string(),),
            })
        );
    }

    #[test]
    fn for_in_calculated_range() {
        let source = Source::unknown("for i in (1 + 1)..5; ls");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Range(RangeFor {
                    receiver: "i",
                    iterable: Iterable::Range(NumericRange {
                        start: Expr::Parenthesis(Parenthesis {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    lexeme: "1",
                                    parsed: 1.into(),
                                })),
                                op: BinaryOperator::Plus,
                                right: Box::new(Expr::Literal(Literal {
                                    lexeme: "1",
                                    parsed: 1.into(),
                                })),
                            }))
                        }),
                        end: Expr::Literal(Literal {
                            lexeme: "5",
                            parsed: 5.into(),
                        }),
                        step: None,
                        upper_inclusive: false,
                    })
                })),
                body: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("ls".into())]
                }))
            })]
        );
    }

    #[test]
    fn for_in_files_range() {
        let source = Source::unknown("for f in * {\n\tfile $f\n}");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Range(RangeFor {
                    receiver: "f",
                    iterable: Iterable::Files(FilePattern {
                        lexeme: "*",
                        pattern: "*".to_owned(),
                    })
                })),
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal("file".into()),
                            Expr::VarReference(VarReference { name: "f" })
                        ]
                    })]
                }))
            })]
        );
    }

    #[test]
    fn classical_for() {
        let source = Source::unknown("for (( var i=0; $i<10; i=$(( $i + 1 )) ))\necho $i");
        let expr = parse(source).expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::For(For {
                kind: Box::new(ForKind::Conditional(ConditionalFor {
                    initializer: Expr::VarDeclaration(VarDeclaration {
                        kind: VarKind::Var,
                        var: TypedVariable {
                            name: "i",
                            ty: None,
                        },
                        initializer: Some(Box::new(Expr::Literal(Literal {
                            lexeme: "0",
                            parsed: 0.into(),
                        }))),
                    }),
                    condition: Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::VarReference(VarReference { name: "i" })),
                        op: BinaryOperator::Less,
                        right: Box::new(Expr::Literal(Literal {
                            lexeme: "10",
                            parsed: 10.into(),
                        }))
                    }),
                    increment: Expr::Assign(Assign {
                        name: "i",
                        value: Box::new(Expr::Parenthesis(Parenthesis {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::VarReference(VarReference { name: "i" })),
                                op: BinaryOperator::Plus,
                                right: Box::new(Expr::Literal(Literal {
                                    lexeme: "1",
                                    parsed: 1.into(),
                                }))
                            }))
                        })),
                    })
                })),
                body: Box::new(Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal("echo".into()),
                        Expr::VarReference(VarReference { name: "i" })
                    ]
                }))
            })]
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
