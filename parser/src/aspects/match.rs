use ast::r#match::MatchPattern::{Literal, Template, VarRef, Wildcard};
use ast::r#match::{Match, MatchArm, MatchPattern};
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};
use lexer::token::TokenType::{
    At, Bar, CurlyLeftBracket, CurlyRightBracket, FatArrow, Identifier, If,
};
use lexer::token::{Token, TokenType};

use crate::aspects::literal::LiteralLeniency;
use crate::err::ParseErrorKind;
use crate::moves::{
    aerated, any, blanks, eox, line_end, not, of_type, of_types, repeat, MoveOperations,
};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    pub(crate) fn parse_match(&mut self) -> ParseResult<Match> {
        let start = self.cursor.force(
            of_type(TokenType::Match),
            "expected 'match' keyword at start of match expression.",
        )?;
        self.cursor.advance(blanks());

        let operand = Box::new(self.statement()?);

        let (arms, segment) = self.parse_match_arms()?;

        Ok(Match {
            operand,
            arms,
            segment: start.span.start..segment.end,
        })
    }
}

impl Parser<'_> {
    fn parse_match_arms(&mut self) -> ParseResult<(Vec<MatchArm>, SourceSegment)> {
        let opening_bracket = self.cursor.force_with(
            blanks().then(of_type(CurlyLeftBracket)),
            "expected match start",
            ParseErrorKind::Expected("{".to_string()),
        )?;

        let mut arms: Vec<MatchArm> = Vec::new();

        while self.cursor.lookahead(blanks().then(eox())).is_none() {
            match self.parse_match_arm() {
                Ok(arm) => arms.push(arm),
                Err(err) => {
                    self.recover_from(err, line_end());
                }
            }
        }
        let closing_bracket = self.cursor.force_with(
            blanks().then(of_type(CurlyRightBracket)),
            "expected '}'",
            ParseErrorKind::Unpaired(opening_bracket.span.clone()),
        )?;

        Ok((arms, opening_bracket.span.start..closing_bracket.span.end))
    }

    fn parse_match_arm(&mut self) -> ParseResult<MatchArm> {
        self.cursor.advance(blanks()); //consume blanks

        let val_name = self.parse_extracted_name()?;
        let patterns = self.parse_patterns()?;
        let guard = self.parse_guard()?;
        let body = self.parse_body()?;

        let mut segment = body.segment();
        segment.start = match val_name {
            Some(ref name) => name.span.clone(),
            None => patterns.first().expect("at least one pattern").segment(),
        }
        .start;

        Ok(MatchArm {
            val_name: val_name
                .map(|name| ast::variable::Identifier::extract(self.source, name.span)),
            patterns,
            guard,
            body,
            segment,
        })
    }

    fn parse_extracted_name(&mut self) -> ParseResult<Option<Token>> {
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
                .map(Some);
            //consume everything before 'at' included
            self.cursor
                .advance(repeat(not(of_type(At)).and_then(any())).then(any()));
            name
        } else {
            Ok(None)
        }
    }

    fn is_at_pattern_end(&self) -> bool {
        self.cursor
            .lookahead(blanks().then(of_types(&[If, FatArrow])))
            .is_some()
    }

    fn parse_patterns(&mut self) -> ParseResult<Vec<MatchPattern>> {
        if self.is_at_pattern_end() {
            return self.expected("required pattern", ParseErrorKind::Unexpected);
        }

        //store start lexeme
        let start = self.cursor.lookahead(blanks()).unwrap().span.start; //blanks always succeeds // TODO check if true
        let first = self.parse_pattern()?;

        let mut patterns = vec![first.clone()];

        while self.cursor.advance(blanks().then(of_type(Bar))).is_some() {
            let pattern = self.parse_pattern()?;
            if let Wildcard(_) = pattern {
                return self.expected("unexpected wildcard", ParseErrorKind::Unexpected);
            }
            patterns.push(pattern)
        }

        if let Wildcard(_) = first {
            return if patterns.len() == 1 {
                Ok(vec![first])
            } else {
                let end = self.cursor.peek().span.end;
                let selection = start..end;
                return self.expected_with(
                    "wildcard pattern cannot be followed by other patterns",
                    selection,
                    ParseErrorKind::Unexpected,
                );
            };
        }

        if !self.is_at_pattern_end() {
            return self.expected(
                "Unexpected token, expected '|', 'if' or '=>'",
                ParseErrorKind::Unexpected,
            );
        }

        Ok(patterns)
    }

    fn parse_pattern(&mut self) -> ParseResult<MatchPattern> {
        self.cursor.advance(blanks()); //consume blanks;

        match self.cursor.peek().token_type {
            TokenType::Star => {
                let star = self.cursor.next()?;
                Ok(Wildcard(star.span))
            }
            _ => match self.literal(LiteralLeniency::Strict)? {
                Expr::Literal(literal) => Ok(Literal(literal)),
                Expr::TemplateString(template) => Ok(Template(template)),
                Expr::VarReference(var_ref) => Ok(VarRef(var_ref)),
                _ => self.expected("unexpected literal", ParseErrorKind::Unexpected),
            },
        }
    }

    fn parse_guard(&mut self) -> ParseResult<Option<Expr>> {
        if self.cursor.advance(aerated(of_type(If))).is_none() {
            return Ok(None);
        }

        self.expression().map(Some)
    }

    fn parse_body(&mut self) -> ParseResult<Expr> {
        self.cursor
            .force(aerated(of_type(FatArrow)), "missing '=>'")?;
        let body = self.statement();
        self.cursor.advance(repeat(line_end()));
        body
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::Call;
    use ast::group::Subshell;
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::r#match::{Match, MatchArm, MatchPattern};
    use ast::test::Test;
    use ast::value::{Literal, TemplateString};
    use ast::variable::{TypedVariable, VarDeclaration, VarKind, VarName, VarReference};
    use ast::Expr;
    use context::source::SourceSegmentHolder;
    use context::str_find::{find_between, find_in, find_in_nth};

    use crate::aspects::literal::literal_expr;
    use crate::err::ParseError;
    use crate::err::ParseErrorKind::Unexpected;
    use crate::parse;
    use crate::source::{identifier, identifier_nth, literal};

    #[test]
    fn parse_match_as_value() {
        let source = "val x = match $1 {
           '-e' => 75
           y@\"test $2\" | 2 | $USER | 't x' => 4 - 7
        }";
        let ast = parse(source).expect("parser failed");

        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: identifier(source, "x"),
                    ty: None,
                },
                initializer: Some(Box::new(Expr::Match(Match {
                    operand: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("1".into()),
                        segment: find_in(source, "$1"),
                    })),
                    arms: vec![
                        MatchArm {
                            val_name: None,
                            patterns: vec![MatchPattern::Literal(Literal {
                                parsed: "-e".into(),
                                segment: find_in(source, "'-e'"),
                            })],
                            guard: None,
                            body: Expr::Literal(Literal {
                                parsed: 75.into(),
                                segment: find_in(source, "75"),
                            }),
                            segment: find_in(source, "'-e' => 75"),
                        },
                        MatchArm {
                            val_name: Some(identifier(source, "y")),
                            patterns: vec![
                                MatchPattern::Template(TemplateString {
                                    parts: vec![
                                        literal(source, "test "),
                                        Expr::VarReference(VarReference {
                                            name: VarName::User("2".into()),
                                            segment: find_in(source, "$2"),
                                        }),
                                    ],
                                    segment: find_in(source, "\"test $2\""),
                                }),
                                MatchPattern::Literal(Literal {
                                    parsed: 2.into(),
                                    segment: find_in_nth(source, "2", 1),
                                }),
                                MatchPattern::VarRef(VarReference {
                                    name: VarName::User("USER".into()),
                                    segment: find_in(source, "$USER"),
                                }),
                                MatchPattern::Literal(Literal {
                                    parsed: "t x".into(),
                                    segment: find_in(source, "'t x'"),
                                }),
                            ],
                            guard: None,
                            body: Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    parsed: 4.into(),
                                    segment: find_in(source, "4")
                                })),
                                op: BinaryOperator::Minus,
                                right: Box::new(Expr::Literal(Literal {
                                    parsed: 7.into(),
                                    segment: find_in_nth(source, "7", 1),
                                })),
                            }),
                            segment: find_in(source, "y@\"test $2\" | 2 | $USER | 't x' => 4 - 7")
                        },
                    ],
                    segment: find_between(source, "match $1", "}"),
                }))),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn parse_complete_match() {
        let source = "match $1 {\
           '-e' => ();;;;;\n;;\n;;;;;\
           y@\"test $2\" | 2 | $USER | 't x' => ()
           x@* if [ $a == 1 ] => ();\
           * => echo $it
        }";
        let ast = parse(source).expect("parse fail");

        assert_eq!(
            ast,
            vec![Expr::Match(Match {
                operand: Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("1".into()),
                    segment: find_in(source, "$1"),
                })),
                arms: vec![
                    MatchArm {
                        val_name: None,
                        patterns: vec![MatchPattern::Literal(literal_expr(source, "'-e'"))],
                        guard: None,
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new(),
                            segment: find_in(source, "()"),
                        }),
                        segment: find_in(source, "'-e' => ()"),
                    },
                    MatchArm {
                        val_name: Some(identifier(source, "y")),
                        patterns: vec![
                            MatchPattern::Template(TemplateString {
                                parts: vec![
                                    literal(source, "test "),
                                    Expr::VarReference(VarReference {
                                        name: VarName::User("2".into()),
                                        segment: find_in(source, "$2"),
                                    }),
                                ],
                                segment: find_in(source, "\"test $2\""),
                            }),
                            MatchPattern::Literal(Literal {
                                parsed: 2.into(),
                                segment: find_in_nth(source, "2", 1),
                            }),
                            MatchPattern::VarRef(VarReference {
                                name: VarName::User("USER".into()),
                                segment: find_in(source, "$USER"),
                            }),
                            MatchPattern::Literal(Literal {
                                parsed: "t x".into(),
                                segment: find_in(source, "'t x'"),
                            }),
                        ],
                        guard: None,
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new(),
                            segment: find_in_nth(source, "()", 1),
                        }),
                        segment: find_in(source, "y@\"test $2\" | 2 | $USER | 't x' => ()"),
                    },
                    MatchArm {
                        val_name: Some(identifier_nth(source, "x", 1)),
                        patterns: vec![MatchPattern::Wildcard(find_in(source, "*"))],
                        guard: Some(Expr::Test(Test {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::VarReference(VarReference {
                                    name: VarName::User("a".into()),
                                    segment: find_in(source, "$a"),
                                })),
                                op: BinaryOperator::EqualEqual,
                                right: Box::new(Expr::Literal(Literal {
                                    parsed: 1.into(),
                                    segment: find_in_nth(source, "1", 1),
                                })),
                            })),
                            segment: find_in(source, "[ $a == 1 ]"),
                        })),
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new(),
                            segment: find_in_nth(source, "()", 2)
                        }),
                        segment: find_in(source, "x@* if [ $a == 1 ] => ()")
                    },
                    MatchArm {
                        val_name: None,
                        patterns: vec![MatchPattern::Wildcard(find_in_nth(source, "*", 1))],
                        guard: None,
                        body: Expr::Call(Call {
                            arguments: vec![
                                literal(source, "echo"),
                                Expr::VarReference(VarReference {
                                    name: VarName::User("it".into()),
                                    segment: find_in(source, "$it"),
                                }),
                            ],
                        }),
                        segment: find_in(source, "* => echo $it")
                    },
                ],
                segment: find_between(source, "match", "}"),
            })]
        )
    }

    #[test]
    fn parse_match_direct_wildcard() {
        let source = "\
        match nginx {\
           * => echo $it
        }\
        ";
        let ast = parse(source).expect("parse fail");

        assert_eq!(
            ast,
            vec![Expr::Match(Match {
                operand: Box::new(Expr::Call(Call {
                    arguments: vec![literal(source, "nginx")],
                })),
                arms: vec![MatchArm {
                    val_name: None,
                    patterns: vec![MatchPattern::Wildcard(find_in(source, "*"))],
                    guard: None,
                    body: Expr::Call(Call {
                        arguments: vec![
                            literal(source, "echo"),
                            Expr::VarReference(VarReference {
                                name: VarName::User("it".into()),
                                segment: find_in(source, "$it"),
                            }),
                        ],
                    }),
                    segment: find_in(source, "* => echo $it")
                },],
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn parse_complete_match_blanks() {
        let source = "\
        match \n\n $1 \n\n {\n\n\
           \n\n '-e' \n\n => \n\n () \n\n\
           \n\n y \n\n @ \n\n \"test $2\" \n\n | 2 \n\n | \n\n $USER \n\n | \n\n 't x' \n\n =>\n\n  () \n\n\
           \n\n x@* \n\n if \n\n [ $a == 1 ]\n\n  =>\n\n  () \n\n\
           \n\n * \n\n => \n\n echo $it \n\n\
        \n\n }\
        ";
        let ast = parse(source).expect("parse fail");

        assert_eq!(
            ast,
            vec![Expr::Match(Match {
                operand: Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("1".into()),
                    segment: find_in(source, "$1"),
                })),
                arms: vec![
                    MatchArm {
                        val_name: None,
                        patterns: vec![MatchPattern::Literal(Literal {
                            parsed: "-e".into(),
                            segment: find_in(source, "'-e'"),
                        })],
                        guard: None,
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new(),
                            segment: find_in(source, "()"),
                        }),
                        segment: find_in(source, "'-e' \n\n => \n\n ()"),
                    },
                    MatchArm {
                        val_name: Some(identifier(source, "y")),
                        patterns: vec![
                            MatchPattern::Template(TemplateString {
                                parts: vec![
                                    literal(source, "test "),
                                    Expr::VarReference(VarReference { name: VarName::User("2".into()), segment: find_in(source, "$2") }),
                                ],
                                segment: find_in(source, "\"test $2\""),
                            }),
                            MatchPattern::Literal(Literal {
                                parsed: 2.into(),
                                segment: find_in_nth(source, "2", 1),
                            }),
                            MatchPattern::VarRef(VarReference {
                                name: VarName::User("USER".into()),
                                segment: find_in(source, "$USER"),
                            }),
                            MatchPattern::Literal(Literal {
                                parsed: "t x".into(),
                                segment: find_in(source, "'t x'"),
                            }),
                        ],
                        guard: None,
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new(),
                            segment: find_in_nth(source, "()", 1),
                        }),
                        segment: find_in(source, "y \n\n @ \n\n \"test $2\" \n\n | 2 \n\n | \n\n $USER \n\n | \n\n 't x' \n\n =>\n\n  ()")
                    },
                    MatchArm {
                        val_name: Some(identifier_nth(source, "x", 1)),
                        patterns: vec![MatchPattern::Wildcard(find_in(source, "*"))],
                        guard: Some(Expr::Test(Test {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::VarReference(VarReference {
                                    name: VarName::User("a".into()),
                                    segment: find_in(source, "$a"),
                                })),
                                op: BinaryOperator::EqualEqual,
                                right: Box::new(Expr::Literal(Literal {
                                    parsed: 1.into(),
                                    segment: find_in_nth(source, "1", 1),
                                })),
                            })),
                            segment: find_in(source, "[ $a == 1 ]"),
                        })),
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new(),
                            segment: find_in_nth(source, "()", 2),
                        }),
                        segment: find_in(source, "x@* \n\n if \n\n [ $a == 1 ]\n\n  =>\n\n  ()")
                    },
                    MatchArm {
                        val_name: None,
                        patterns: vec![MatchPattern::Wildcard(find_in_nth(source, "*", 1))],
                        guard: None,
                        body: Expr::Call(Call {
                            arguments: vec![
                                literal(source, "echo"),
                                Expr::VarReference(VarReference {
                                    name: VarName::User("it".into()),
                                    segment: find_in(source, "$it"),
                                }),
                            ],
                        }),
                        segment: find_in(source, "* \n\n => \n\n echo $it")
                    },
                ],
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn match_patterns_with_wildcard() {
        let src = "\
        match $1 {\
           '-e' | * => ()\
        }\
        ";
        let res = parse(src).errors;
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
           * | 'x' | 'y' => ()\
        }\
        ";
        let res = parse(src).errors;
        assert_eq!(
            res,
            vec![ParseError {
                message: "wildcard pattern cannot be followed by other patterns".to_string(),
                kind: Unexpected,
                position: find_in(src, "* | 'x' | 'y' "),
            }]
        )
    }

    #[test]
    fn match_patterns_missing_bar() {
        let src = "\
        match $1 {\
           'x' 'y' => ()\
        }\
        ";
        let res = parse(src).errors;
        assert_eq!(
            res,
            vec![ParseError {
                message: "Unexpected token, expected '|', 'if' or '=>'".to_owned(),
                kind: Unexpected,
                position: src.find('y').map(|i| i - 2..i - 1).unwrap(),
            }]
        )
    }

    #[test]
    fn match_patterns_trailing_bar() {
        let src = "\
        match $1 {\
           'x' | => ()\
        }\
        ";
        let res = parse(src).errors;
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
           'x' | 'y' ()\
        }\
        ";
        let res = parse(src).errors;
        assert_eq!(
            res,
            vec![ParseError {
                message: "Unexpected token, expected '|', 'if' or '=>'".to_owned(),
                kind: Unexpected,
                position: src.find('(').map(|i| i - 1..i).unwrap(),
            }]
        )
    }
}
