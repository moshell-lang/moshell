use lexer::token::TokenType;
use lexer::token::TokenType::{
    At, Bar, CurlyLeftBracket, CurlyRightBracket, FatArrow, Identifier, If,
};

use crate::aspects::literal::{LiteralAspect, LiteralLeniency};
use crate::err::ParseErrorKind;
use crate::moves::{
    aerated, any, blanks, eod, eox, not, of_type, of_types, repeat, MoveOperations,
};
use crate::parser::{ParseResult, Parser};
use ast::r#match::MatchPattern::{Literal, Template, VarRef, Wildcard};
use ast::r#match::{Match, MatchArm, MatchPattern};
use ast::Expr;

/// A Parser Aspect for match expression-statement and value
pub trait MatchAspect<'a> {
    ///parse a match statement, input parser determines how to parse each arm body
    fn parse_match<P>(&mut self, parse_arm: P) -> ParseResult<Match<'a>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'a>> + Clone;
}

impl<'a> MatchAspect<'a> for Parser<'a> {
    fn parse_match<P>(&mut self, parse_arm: P) -> ParseResult<Match<'a>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'a>> + Clone,
    {
        let start = self.cursor.force(
            of_type(TokenType::Match).and_then(blanks()),
            "expected 'match' keyword at start of match expression.",
        )?;

        let operand = Box::new(self.expression_statement()?);

        let arms = self.parse_match_arms(parse_arm)?;

        Ok(Match {
            operand,
            arms,
            segment: self.cursor.relative_pos(start).start..self.cursor.re,
        })
    }
}

impl<'a> Parser<'a> {
    fn parse_match_arms<P>(&mut self, parse_arm: P) -> ParseResult<Vec<MatchArm<'a>>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'a>> + Clone,
    {
        let opening_bracket = self.cursor.force_with(
            blanks().then(of_type(CurlyLeftBracket)),
            "expected match start",
            ParseErrorKind::Expected("{".to_string()),
        )?;
        self.delimiter_stack.push_back(opening_bracket.clone());

        let mut arms: Vec<MatchArm<'a>> = Vec::new();

        while self.cursor.lookahead(blanks().then(eod())).is_none() {
            let arm = self.parse_match_arm(parse_arm.clone())?;
            arms.push(arm);
        }
        self.cursor.force_with(
            blanks().then(of_type(CurlyRightBracket)),
            "expected '}'",
            ParseErrorKind::Unpaired(self.cursor.relative_pos(opening_bracket)),
        )?;
        self.delimiter_stack.pop_back();

        Ok(arms)
    }

    fn parse_match_arm<P>(&mut self, parse_arm: P) -> ParseResult<MatchArm<'a>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'a>>,
    {
        self.cursor.advance(blanks()); //consume blanks

        let start = self.cursor.peek();

        let val_name = self.parse_extracted_name()?;
        let patterns = self.parse_patterns()?;
        let guard = self.parse_guard()?;
        let body = self.parse_body(parse_arm)?;

        let end = self.cursor.peek();

        let segment = start..end;

        Ok(MatchArm {
            val_name,
            patterns,
            guard,
            body,
            segment
        })
    }

    fn parse_extracted_name(&mut self) -> ParseResult<Option<&'a str>> {
        if self
            .cursor
            .lookahead(aerated(any()).then(of_type(At)))
            .is_some()
        {
            let name = self
                .cursor
                .force(
                    of_type(Identifier),
                    "expected identifier for extracted val name",
                )
                .map(|t| t.value)
                .map(Some);
            //consume everything before 'at' included
            self.cursor
                .advance(repeat(not(of_type(At)).and_then(any())).then(any()));
            name
        } else {
            Ok(None)
        }
    }

    fn parse_patterns(&mut self) -> ParseResult<Vec<MatchPattern<'a>>> {
        macro_rules! is_at_pattern_end {
            () => {
                self.cursor
                    .lookahead(blanks().then(of_types(&[If, FatArrow])))
                    .is_some()
            };
        }

        if is_at_pattern_end!() {
            return self.expected("required pattern", ParseErrorKind::Unexpected);
        }

        //store start lexeme
        let start = self.cursor.lookahead(blanks()).unwrap().value; //blanks always succeeds
        let first = self.parse_pattern()?;

        let mut patterns = vec![first.clone()];

        while self.cursor.advance(blanks().then(of_type(Bar))).is_some() {
            let pattern = self.parse_pattern()?;
            if pattern == Wildcard {
                return self.expected("unexpected wildcard", ParseErrorKind::Unexpected);
            }
            patterns.push(pattern)
        }

        if first == Wildcard {
            return if patterns.len() == 1 {
                Ok(vec![first])
            } else {
                let start = self.cursor.relative_pos(start).start;
                let end = self.cursor.relative_pos(self.cursor.peek()).end;
                let selection = start..end;
                Err(self.mk_parse_error(
                    "wildcard pattern cannot be followed by other patterns",
                    &self.source.source[selection],
                    ParseErrorKind::Unexpected,
                ))
            };
        }

        if !is_at_pattern_end!() {
            let token = self.cursor.lookahead(blanks().then(any())).unwrap().value;
            return self.expected(
                &format!("unexpected token, expected '|', 'if' or '=>', found '{token}'"),
                ParseErrorKind::Unexpected,
            );
        }

        Ok(patterns)
    }

    fn parse_pattern(&mut self) -> ParseResult<MatchPattern<'a>> {
        self.cursor.advance(blanks()); //consume blanks;

        match self.cursor.peek().token_type {
            TokenType::Star => {
                self.cursor.next()?;
                Ok(Wildcard)
            }
            _ => match self.literal(LiteralLeniency::Strict)? {
                Expr::Literal(literal) => Ok(Literal(literal)),
                Expr::TemplateString(template) => Ok(Template(template)),
                Expr::VarReference(var_ref) => Ok(VarRef(var_ref)),
                _ => self.expected("unexpected literal", ParseErrorKind::Unexpected),
            },
        }
    }

    fn parse_guard(&mut self) -> ParseResult<Option<Expr<'a>>> {
        if self.cursor.advance(aerated(of_type(If))).is_none() {
            return Ok(None);
        }

        self.expression().map(Some)
    }

    fn parse_body<P>(&mut self, mut parse_arm: P) -> ParseResult<Expr<'a>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'a>>,
    {
        self.cursor
            .force(aerated(of_type(FatArrow)), "missing '=>'")?;
        let body = parse_arm(self);
        self.cursor.advance(repeat(eox()));
        body
    }
}

#[cfg(test)]
mod tests {
    use context::source::{Source, SourceSegmentHolder};
    use pretty_assertions::assert_eq;

    use crate::err::ParseErrorKind::Unexpected;
    use crate::err::{find_between, find_in, find_in_nth, ParseError};
    use crate::parse;
    use ast::call::Call;
    use ast::group::Subshell;
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::r#match::{Match, MatchArm, MatchPattern};
    use ast::test::Test;
    use ast::value::{Literal, TemplateString};
    use ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
    use ast::Expr;

    #[test]
    fn parse_match_as_value() {
        let source = Source::unknown(
            "val x = match $1 {
           -e => 75
           y@\"test $2\" | 2 | $USER | 't x' => 4 - 7
        }",
        );
        let ast = parse(source.clone()).expect("parser failed");

        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "x",
                    ty: None,
                    segment: Default::default(),
                },
                initializer: Some(Box::new(Expr::Match(Match {
                    operand: Box::new(Expr::VarReference(VarReference {
                        name: "1",
                        segment: Default::default(),
                    })),
                    arms: vec![
                        MatchArm {
                            val_name: None,
                            patterns: vec![MatchPattern::Literal(Literal {
                                parsed: "-e".into(),
                                segment: find_in(source.source, "-e"),
                            })],
                            guard: None,
                            body: Expr::Literal(Literal {
                                parsed: 75.into(),
                                segment: find_in(source.source, "75"),
                            }),
                            segment: find_in(content, "-e => 75"),
                        },
                        MatchArm {
                            val_name: Some("y"),
                            patterns: vec![
                                MatchPattern::Template(TemplateString {
                                    parts: vec![
                                        Expr::Literal("test ".into()),
                                        Expr::VarReference(VarReference {
                                            name: "2",
                                            segment: find_in(source.source, "$2"),
                                        }),
                                    ]
                                }),
                                MatchPattern::Literal(Literal {
                                    parsed: 2.into(),
                                    segment: find_in_nth(source.source, "2", 1),
                                }),
                                MatchPattern::VarRef(VarReference {
                                    name: "USER",
                                    segment: find_in(source.source, "USER"),
                                }),
                                MatchPattern::Literal(Literal {
                                    parsed: "t x".into(),
                                    segment: find_in(source.source, "'t x'"),
                                }),
                            ],
                            guard: None,
                            body: Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    parsed: 4.into(),
                                    segment: find_in(source.source, "4")
                                })),
                                op: BinaryOperator::Minus,
                                right: Box::new(Expr::Literal(Literal {
                                    parsed: 7.into(),
                                    segment: find_in(source.source, "7"),
                                })),
                            }),
                            segment: find_in(source.source, "y@\"test $2\" | 2 | $USER | 't x' => 4 - 7")
                        },
                    ],
                    segment: Default::default(),
                }))),
                segment: source.segment(),
            }),]
        )
    }

    #[test]
    fn parse_complete_match() {
        let content = "\
        match $1 {\
           -e => ();;;;;\n;;\n;;;;;\
           y@\"test $2\" | 2 | $USER | 't x' => ()
           x@* if [ $a == 1 ] => ();\
           * => echo $it
        }\
        ";
        let ast = parse(Source::unknown(content)).expect("parse fail");

        assert_eq!(
            ast,
            vec![Expr::Match(Match {
                operand: Box::new(Expr::VarReference(VarReference {
                    name: "1",
                    segment: Default::default(),
                })),
                arms: vec![
                    MatchArm {
                        val_name: None,
                        patterns: vec![MatchPattern::Literal(Literal {
                            parsed: "-e",
                            segment: find_in(content, "-e"),
                        })],
                        guard: None,
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new(),
                            segment: find_in(content, "()"),
                        }),
                        segment: find_in(content, "-e => ()"),
                    },
                    MatchArm {
                        val_name: Some("y"),
                        patterns: vec![
                            MatchPattern::Template(TemplateString {
                                parts: vec![
                                    Expr::Literal("test ".into()),
                                    Expr::VarReference(VarReference {
                                        name: "2",
                                        segment: Default::default(),
                                    }),
                                ]
                            }),
                            MatchPattern::Literal(Literal {
                                parsed: 2.into(),
                                segment: find_in_nth(content, "2", 1),
                            }),
                            MatchPattern::VarRef(VarReference {
                                name: "USER",
                                segment: find_in(content, "USER"),
                            }),
                            MatchPattern::Literal(Literal {
                                parsed: "t x".into(),
                                segment: find_in(content, "'t x'"),
                            }),
                        ],
                        guard: None,
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new(),
                            segment: find_in_nth(content, "()", 1),
                        }),
                        segment: find_in(content, "y@\"test $2\" | 2 | $USER | 't x' => ()"),
                    },
                    MatchArm {
                        val_name: Some("x"),
                        patterns: vec![MatchPattern::Wildcard],
                        guard: Some(Expr::Test(Test {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::VarReference(VarReference {
                                    name: "a",
                                    segment: Default::default(),
                                })),
                                op: BinaryOperator::EqualEqual,
                                right: Box::new(Expr::Literal(Literal {
                                    parsed: 1.into(),
                                    segment: find_in_nth(content, "1", 1),
                                })),
                            })),
                            segment: find_in(content, "[ $a == 1 ]"),
                        })),
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new(),
                            segment: find_in_nth(content, "()", 2)
                        }),
                        segment: find_in(content, "x@* if [ $a == 1 ] => ()")
                    },
                    MatchArm {
                        val_name: None,
                        patterns: vec![MatchPattern::Wildcard(find_in_nth(content, "*", 1))],
                        guard: None,
                        body: Expr::Call(Call {
                            arguments: vec![
                                Expr::Literal("echo".into()),
                                Expr::VarReference(VarReference {
                                    name: "it",
                                    segment: find_in(content, "$it"),
                                }),
                            ],
                            type_parameters: vec![],
                        }),
                        segment: find_in(content, "* => echo $it")
                    },
                ],
                segment: find_between(content, "match", "}"),
            })]
        )
    }

    #[test]
    fn parse_match_direct_wildcard() {
        let ast = parse(Source::unknown(
            "\
        match nginx {\
           * => echo $it
        }\
        ",
        ))
        .expect("parse fail");

        assert_eq!(
            ast,
            vec![Expr::Match(Match {
                operand: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("nginx".into())],
                    type_parameters: vec![],
                })),
                arms: vec![MatchArm {
                    val_name: None,
                    patterns: vec![MatchPattern::Wildcard],
                    guard: None,
                    body: Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal("echo".into()),
                            Expr::VarReference(VarReference {
                                name: "it",
                                segment: find_in(content, "$it"),
                            }),
                        ],
                        type_parameters: vec![],
                    }),
                    segment: find_in(content, "$a")
                },],
                segment: Default::default(),
            })]
        )
    }

    #[test]
    fn parse_complete_match_blanks() {
        let content = "\
        match \n\n $1 \n\n {\n\n\
           \n\n -e \n\n => \n\n () \n\n\
           \n\n y \n\n @ \n\n \"test $2\" \n\n | 2 \n\n | \n\n $USER \n\n | \n\n 't x' \n\n =>\n\n  () \n\n\
           \n\n x@* \n\n if \n\n [ $a == 1 ]\n\n  =>\n\n  () \n\n\
           \n\n * \n\n => \n\n echo $it \n\n\
        \n\n }\
        ";
        let ast = parse(Source::unknown(content)).expect("parse fail");

        assert_eq!(
            ast,
            vec![Expr::Match(Match {
                operand: Box::new(Expr::VarReference(VarReference {
                    name: "1",
                    segment: find_in(content, "$1"),
                })),
                arms: vec![
                    MatchArm {
                        val_name: None,
                        patterns: vec![MatchPattern::Literal(Literal {
                            parsed: "-e".into(),
                            segment: find_in(content, "-e"),
                        })],
                        guard: None,
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new(),
                            segment: find_in(content, "()"),
                        }),
                        segment: find_in(content, "-e \n\n => \n\n ()"),
                    },
                    MatchArm {
                        val_name: Some("y"),
                        patterns: vec![
                            MatchPattern::Template(TemplateString {
                                parts: vec![
                                    Expr::Literal(Literal {
                                        parsed: "test",
                                        segment: find_in(content, "test"),
                                    }),
                                    Expr::VarReference(VarReference {
                                        name: "2",
                                        segment: find_in(content, "$2"),
                                    }),
                                ]
                            }),
                            MatchPattern::Literal(Literal {
                                parsed: 2.into(),
                                segment: find_in_nth(content, "2", 1),
                            }),
                            MatchPattern::VarRef(VarReference {
                                name: "USER",
                                segment: find_in(content, "USER"),
                            }),
                            MatchPattern::Literal(Literal {
                                parsed: "t x".into(),
                                segment: find_in(content, "'t x'"),
                            }),
                        ],
                        guard: None,
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new(),
                            segment: find_in_nth(content, "()", 1),
                        }),
                        segment: find_in(content, "y \n\n @ \n\n \"test $2\" \n\n | 2 \n\n | \n\n $USER \n\n | \n\n 't x' \n\n =>\n\n  ()")
                    },
                    MatchArm {
                        val_name: Some("x"),
                        patterns: vec![MatchPattern::Wildcard],
                        guard: Some(Expr::Test(Test {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::VarReference(VarReference {
                                    name: "a",
                                    segment: find_in(content, "$a"),
                                })),
                                op: BinaryOperator::EqualEqual,
                                right: Box::new(Expr::Literal(Literal {
                                    parsed: 1.into(),
                                    segment: find_in_nth(content, "1", 1),
                                })),
                            })),
                            segment: find_in(content, "[ $a == 1 ]"),
                        })),
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new(),
                            segment: find_in_nth(content, "()", 2),
                        }),
                        segment: find_in(content, "x@* \n\n if \n\n [ $a == 1 ]\n\n  =>\n\n  ()")
                    },
                    MatchArm {
                        val_name: None,
                        patterns: vec![MatchPattern::Wildcard],
                        guard: None,
                        body: Expr::Call(Call {
                            arguments: vec![
                                Expr::Literal("echo".into()),
                                Expr::VarReference(VarReference {
                                    name: "it",
                                    segment: Default::default(),
                                }),
                            ],
                            type_parameters: vec![],
                        }),
                        segment: find_in(content, "* \n\n => \n\n echo $it")
                    },
                ],
                segment: Default::default(),
            })]
        )
    }

    #[test]
    fn match_patterns_with_wildcard() {
        let src = "\
        match $1 {\
           -e | * => ()\
        }\
        ";
        let res = parse(Source::unknown(src)).errors;
        assert_eq!(
            res,
            vec![ParseError {
                message: "unexpected wildcard".to_string(),
                position: src.find('*').map(|i| i + 1..i + 2).unwrap(),
                kind: Unexpected,
            }]
        )
    }

    #[test]
    fn match_wildcard_with_patterns() {
        let src = "\
        match $1 {\
           * | x | y => ()\
        }\
        ";
        let res = parse(Source::unknown(src)).errors;
        assert_eq!(
            res,
            vec![ParseError {
                message: "wildcard pattern cannot be followed by other patterns".to_string(),
                kind: Unexpected,
                position: src
                    .find("* | x | y ")
                    .map(|i| i..i + "* | x | y ".len())
                    .unwrap(),
            }]
        )
    }

    #[test]
    fn match_patterns_missing_bar() {
        let src = "\
        match $1 {\
           x y => ()\
        }\
        ";
        let res = parse(Source::unknown(src)).errors;
        assert_eq!(
            res,
            vec![ParseError {
                message: "unexpected token, expected '|', 'if' or '=>', found 'y'".to_string(),
                kind: Unexpected,
                position: src.find('y').map(|i| i - 1..i).unwrap(),
            }]
        )
    }

    #[test]
    fn match_patterns_trailing_bar() {
        let src = "\
        match $1 {\
           x | => ()\
        }\
        ";
        let res = parse(Source::unknown(src)).errors;
        assert_eq!(
            res,
            vec![ParseError {
                message: "Unexpected token '=>'.".to_string(),
                position: src.find("=>").map(|i| i..i + 2).unwrap(),
                kind: Unexpected,
            }]
        )
    }

    #[test]
    fn match_patterns_missing_fatarrow() {
        let src = "\
        match $1 {\
           x | y ()\
        }\
        ";
        let res = parse(Source::unknown(src)).errors;
        assert_eq!(
            res,
            vec![ParseError {
                message: "unexpected token, expected '|', 'if' or '=>', found '('".to_string(),
                kind: Unexpected,
                position: src.find('(').map(|i| i - 1..i).unwrap(),
            }]
        )
    }
}
