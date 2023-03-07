use lexer::token::TokenType;
use lexer::token::TokenType::{
    At, Bar, CurlyLeftBracket, CurlyRightBracket, FatArrow, Identifier, If,
};

use crate::aspects::literal::LiteralAspect;
use crate::ast::r#match::MatchPattern::{Literal, Template, VarRef, Wildcard};
use crate::ast::r#match::{Match, MatchArm, MatchPattern};
use crate::ast::Expr;
use crate::err::ParseErrorKind;
use crate::moves::{
    aerated, any, blanks, eod, eox, not, of_type, of_types, repeat, MoveOperations,
};
use crate::parser::{ParseResult, Parser};

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
        self.cursor.force(
            of_type(TokenType::Match).and_then(blanks()),
            "expected 'match' keyword at start of match expression.",
        )?;

        let operand = Box::new(self.expression_statement()?);

        let arms = self.parse_match_arms(parse_arm)?;

        Ok(Match { operand, arms })
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
            ParseErrorKind::Excepted("{"),
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

        let val_name = self.parse_extracted_name()?;
        let patterns = self.parse_patterns()?;
        let guard = self.parse_guard()?;
        let body = self.parse_body(parse_arm)?;

        Ok(MatchArm {
            val_name,
            patterns,
            guard,
            body,
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
            return self.expected(&format!(
                "unexpected token, expected '|', 'if' or '=>', found '{token}'"
            ), ParseErrorKind::Unexpected);
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
            _ => match self.literal()? {
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
    use pretty_assertions::assert_eq;
    use context::source::Source;


    use crate::ast::callable::Call;
    use crate::ast::group::Subshell;
    use crate::ast::operation::{BinaryOperation, BinaryOperator};
    use crate::ast::r#match::{Match, MatchArm, MatchPattern};
    use crate::ast::test::Test;
    use crate::ast::value::{Literal, TemplateString};
    use crate::ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
    use crate::ast::Expr;
    use crate::err::{ParseError};
    use crate::err::ParseErrorKind::Unexpected;
    use crate::parse;

    #[test]
    fn parse_match_as_value() {
        let source = Source::unknown("
        val x = match $1 {
           -e => 75
           y@\"test $2\" | 2 | $USER | 't x' => 4 - 7
        }
        ");
        let ast = parse(source).expect("parser failed");

        assert_eq!(
            ast,
            vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "x",
                    ty: None,
                },
                initializer: Some(Box::new(Expr::Match(Match {
                    operand: Box::new(Expr::VarReference(VarReference { name: "1" })),
                    arms: vec![
                        MatchArm {
                            val_name: None,
                            patterns: vec![MatchPattern::Literal("-e".into())],
                            guard: None,
                            body: Expr::Literal(Literal {
                                lexeme: "75",
                                parsed: 75.into(),
                            }),
                        },
                        MatchArm {
                            val_name: Some("y"),
                            patterns: vec![
                                MatchPattern::Template(TemplateString {
                                    parts: vec![
                                        Expr::Literal("test ".into()),
                                        Expr::VarReference(VarReference { name: "2" }),
                                    ]
                                }),
                                MatchPattern::Literal(Literal {
                                    lexeme: "2",
                                    parsed: 2.into(),
                                }),
                                MatchPattern::VarRef(VarReference { name: "USER" }),
                                MatchPattern::Literal(Literal {
                                    lexeme: "'t x'",
                                    parsed: "t x".into(),
                                }),
                            ],
                            guard: None,
                            body: Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::Literal(Literal {
                                    lexeme: "4",
                                    parsed: 4.into(),
                                })),
                                op: BinaryOperator::Minus,
                                right: Box::new(Expr::Literal(Literal {
                                    lexeme: "7",
                                    parsed: 7.into(),
                                })),
                            }),
                        },
                    ],
                }))),
            }),]
        )
    }

    #[test]
    fn parse_complete_match() {
        let ast = parse(Source::unknown("\
        match $1 {\
           -e => ();;;;;\n;;\n;;;;;\
           y@\"test $2\" | 2 | $USER | 't x' => ()
           x@* if [ $a == 1 ] => ();\
           * => echo $it
        }\
        "))
            .expect("parse fail");

        assert_eq!(
            ast,
            vec![Expr::Match(Match {
                operand: Box::new(Expr::VarReference(VarReference { name: "1" })),
                arms: vec![
                    MatchArm {
                        val_name: None,
                        patterns: vec![MatchPattern::Literal("-e".into())],
                        guard: None,
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new()
                        }),
                    },
                    MatchArm {
                        val_name: Some("y"),
                        patterns: vec![
                            MatchPattern::Template(TemplateString {
                                parts: vec![
                                    Expr::Literal("test ".into()),
                                    Expr::VarReference(VarReference { name: "2" }),
                                ]
                            }),
                            MatchPattern::Literal(Literal {
                                lexeme: "2",
                                parsed: 2.into(),
                            }),
                            MatchPattern::VarRef(VarReference { name: "USER" }),
                            MatchPattern::Literal(Literal {
                                lexeme: "'t x'",
                                parsed: "t x".into(),
                            }),
                        ],
                        guard: None,
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new()
                        }),
                    },
                    MatchArm {
                        val_name: Some("x"),
                        patterns: vec![MatchPattern::Wildcard],
                        guard: Some(Expr::Test(Test {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::VarReference(VarReference { name: "a" })),
                                op: BinaryOperator::EqualEqual,
                                right: Box::new(Expr::Literal(Literal {
                                    lexeme: "1",
                                    parsed: 1.into(),
                                })),
                            }))
                        })),
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new()
                        }),
                    },
                    MatchArm {
                        val_name: None,
                        patterns: vec![MatchPattern::Wildcard],
                        guard: None,
                        body: Expr::Call(Call {
                            arguments: vec![
                                Expr::Literal("echo".into()),
                                Expr::VarReference(VarReference { name: "it" }),
                            ]
                        }),
                    },
                ],
            })]
        )
    }

    #[test]
    fn parse_match_direct_wildcard() {
        let ast = parse(Source::unknown("\
        match nginx {\
           * => echo $it
        }\
        "))
            .expect("parse fail");

        assert_eq!(
            ast,
            vec![Expr::Match(Match {
                operand: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("nginx".into())]
                })),
                arms: vec![MatchArm {
                    val_name: None,
                    patterns: vec![MatchPattern::Wildcard],
                    guard: None,
                    body: Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal("echo".into()),
                            Expr::VarReference(VarReference { name: "it" }),
                        ]
                    }),
                },],
            })]
        )
    }

    #[test]
    fn parse_complete_match_blanks() {
        let ast = parse(Source::unknown("\
        match \n\n $1 \n\n {\n\n\
           \n\n -e \n\n => \n\n () \n\n\
           \n\n y \n\n @ \n\n \"test $2\" \n\n | 2 \n\n | \n\n $USER \n\n | \n\n 't x' \n\n =>\n\n  () \n\n\
           \n\n x@* \n\n if \n\n [ $a == 1 ]\n\n  =>\n\n  () \n\n\
           \n\n * \n\n => \n\n echo $it \n\n\
        \n\n }\
        ")).expect("parse fail");

        assert_eq!(
            ast,
            vec![Expr::Match(Match {
                operand: Box::new(Expr::VarReference(VarReference { name: "1" })),
                arms: vec![
                    MatchArm {
                        val_name: None,
                        patterns: vec![MatchPattern::Literal("-e".into())],
                        guard: None,
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new()
                        }),
                    },
                    MatchArm {
                        val_name: Some("y"),
                        patterns: vec![
                            MatchPattern::Template(TemplateString {
                                parts: vec![
                                    Expr::Literal("test ".into()),
                                    Expr::VarReference(VarReference { name: "2" }),
                                ]
                            }),
                            MatchPattern::Literal(Literal {
                                lexeme: "2",
                                parsed: 2.into(),
                            }),
                            MatchPattern::VarRef(VarReference { name: "USER" }),
                            MatchPattern::Literal(Literal {
                                lexeme: "'t x'",
                                parsed: "t x".into(),
                            }),
                        ],
                        guard: None,
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new()
                        }),
                    },
                    MatchArm {
                        val_name: Some("x"),
                        patterns: vec![MatchPattern::Wildcard],
                        guard: Some(Expr::Test(Test {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::VarReference(VarReference { name: "a" })),
                                op: BinaryOperator::EqualEqual,
                                right: Box::new(Expr::Literal(Literal {
                                    lexeme: "1",
                                    parsed: 1.into(),
                                })),
                            }))
                        })),
                        body: Expr::Subshell(Subshell {
                            expressions: Vec::new()
                        }),
                    },
                    MatchArm {
                        val_name: None,
                        patterns: vec![MatchPattern::Wildcard],
                        guard: None,
                        body: Expr::Call(Call {
                            arguments: vec![
                                Expr::Literal("echo".into()),
                                Expr::VarReference(VarReference { name: "it" }),
                            ]
                        }),
                    },
                ],
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
            vec![
                ParseError {
                    message: "unexpected wildcard".to_string(),
                    position: src.find('*').map(|i| i+1..i+2).unwrap(),
                    kind: Unexpected,
                }
            ]
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
                position: src.find("* | x | y ").map(|i| i..i + "* | x | y ".len()).unwrap(),
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
                position: src.find('y').map(|i| i-1..i).unwrap(),
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
            vec![
                ParseError {
                    message: "Unexpected token '=>'.".to_string(),
                    position: src.find("=>").map(|i| i..i + 2).unwrap(),
                    kind: Unexpected,
                }
            ]
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
                position: src.find('(').map(|i| i-1..i).unwrap(),
            }]
        )
    }
}
