use lexer::token::TokenType;
use lexer::token::TokenType::{At, CurlyLeftBracket, CurlyRightBracket, FatArrow, Identifier, Bar, If};

use crate::aspects::literal::LiteralAspect;
use crate::ast::Expr;
use crate::ast::r#match::{Match, MatchArm, MatchPattern};
use crate::ast::r#match::MatchPattern::{Literal, Template, VarRef, Wildcard};
use crate::moves::{aerated, any, blank, blanks, MoveOperations, not, of_type, of_types, repeat};
use crate::parser::{Parser, ParseResult};

pub trait MatchAspect<'a> {
    fn parse_match<P>(&mut self, parse_arm: P) -> ParseResult<Match<'a>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>> + Clone;
}

impl<'a> MatchAspect<'a> for Parser<'a> {
    fn parse_match<P>(&mut self, parse_arm: P) -> ParseResult<Match<'a>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>> + Clone {
        self.cursor.force(
            of_type(TokenType::Match),
            "expected 'match' keyword at start of match expression.",
        )?;

        let operand = Box::new(self.expression_statement()?);

        let arms = self.parse_match_arms(parse_arm)?;

        Ok(Match {
            operand,
            arms
        })
    }
}

impl<'a> Parser<'a> {
    fn parse_match_arms<P>(&mut self, parse_arm: P) -> ParseResult<Vec<MatchArm<'a>>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>> + Clone {
        self.cursor.force(repeat(blank().then(of_type(CurlyLeftBracket))), "expected '{'")?;

        let mut arms: Vec<MatchArm<'a>> = Vec::new();

        while self.cursor.advance(blanks().then(of_type(CurlyRightBracket))).is_none() {
            let arm = self.parse_match_arm(parse_arm.clone())?;
            arms.push(arm);
        }

        Ok(arms)
    }

    fn parse_match_arm<P>(&mut self, parse_arm: P) -> ParseResult<MatchArm<'a>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>> {
        self.cursor.advance(blanks()); //consume blanks

        let val_name = self.parse_extracted_name()?;
        let patterns = self.parse_patterns()?;
        let guard = self.parse_guard()?;
        let body = Box::new(self.parse_body(parse_arm)?);

        Ok(MatchArm {
            val_name,
            patterns,
            guard,
            body,
        })
    }

    fn parse_extracted_name(&mut self) -> ParseResult<Option<&'a str>> {
        if self.cursor.lookahead(
            aerated(any()).then(of_type(At))
        ).is_some() {
            let name = self.cursor.force(of_type(Identifier), "expected identifier for extracted val name")
                .map(|t| t.value)
                .map(Some);
            //consume everything before 'at' included
            self.cursor.advance(repeat(not(of_type(At))).then(any()));
            name
        } else {
            Ok(None)
        }
    }

    fn parse_patterns(&mut self) -> ParseResult<Vec<MatchPattern<'a>>> {
        macro_rules! is_at_pattern_end {
            () =>{ self.cursor.lookahead(blanks().then(of_types(&[If, FatArrow]))).is_some() }
        }

        if is_at_pattern_end!() {
            return self.expected("required pattern")
        }

        let first = self.parse_pattern()?;

        if first == Wildcard {
            return if !is_at_pattern_end!() {
                self.expected("wildcard pattern cannot be followed with other patterns")
            } else {
                Ok(vec![first])
            }
        }

        let mut patterns = vec![first];

        while self.cursor.advance(blanks().then(of_type(Bar))).is_some() {
            let pattern = self.parse_pattern()?;
            if pattern == Wildcard {
                return self.expected("unexpected wildcard");
            }
            patterns.push(pattern)
        }

        if !is_at_pattern_end!() {
            let token = self.cursor.peek().value;
            return self.expected(&format!("unexpected token, expected 'if' or '=>', found '{token}'"))
        }

        Ok(patterns)
    }


    fn parse_pattern(&mut self) -> ParseResult<MatchPattern<'a>> {
        self.cursor.advance(blanks()); //consume blanks;

        match self.cursor.peek().token_type {
            TokenType::Star => {
                self.cursor.next()?;
                Ok(Wildcard)
            },
            _ => match self.literal()? {
                Expr::Literal(literal) => Ok(Literal(literal)),
                Expr::TemplateString(template) => Ok(Template(template)),
                Expr::VarReference(var_ref) => Ok(VarRef(var_ref)),
                _ => self.expected("unexpected literal") //TODO make a proposal to make AST more typed
            }
        }
    }

    fn parse_guard(&mut self) -> ParseResult<Option<Expr<'a>>> {
        if self.cursor.advance(blanks().then(of_type(If))).is_none() {
            return Ok(None)
        }

        self.expression().map(Some)
    }

    fn parse_body<P>(&mut self, mut parse_arm: P) -> ParseResult<Expr<'a>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>> {
        self.cursor.force(blanks().then(of_type(FatArrow)), "missing '=>'")?;
        parse_arm(self)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use lexer::lexer::lex;
    use crate::ast::callable::Call;
    use crate::ast::Expr;
    use crate::ast::group::Subshell;
    use crate::ast::operation::{BinaryOperation, BinaryOperator};
    use crate::ast::r#match::{Match, MatchArm, MatchPattern};
    use crate::ast::test::Test;
    use crate::ast::value::{Literal, TemplateString};
    use crate::ast::variable::VarReference;
    use crate::parse;

    #[test]
    fn parse_complete_match() {
        let ast = parse(lex("\
        match $1 {\
           -e => ()
           y@\"test $2\" | 2 | $USER | 't x' => ()
           x@* if [ $a == 1 ] => ()
           * => echo $it
        }\
        ")).expect("parse fail");

        assert_eq!(
            ast,
            vec![
                Expr::Match(Match {
                    operand: Box::new(Expr::VarReference(VarReference {
                        name: "1"
                    })),
                    arms: vec![
                        MatchArm {
                            val_name: None,
                            patterns: vec![
                                MatchPattern::Literal("-e".into())
                            ],
                            guard: None,
                            body: Box::new(Expr::Subshell(Subshell {
                                expressions: Vec::new()
                            })),
                        },
                        MatchArm {
                            val_name: Some("y"),
                            patterns: vec![
                                MatchPattern::Template(TemplateString {
                                    parts: vec![
                                        Expr::Literal("test ".into()),
                                        Expr::VarReference(VarReference {
                                            name: "2"
                                        }),
                                    ]
                                }),
                                MatchPattern::Literal(Literal {
                                    lexeme: "2",
                                    parsed: 2.into(),
                                }),
                                MatchPattern::VarRef(VarReference {
                                    name: "USER"
                                }),
                                MatchPattern::Literal(Literal {
                                    lexeme: "'t x'",
                                    parsed: "t x".into(),
                                }),
                            ],
                            guard: None,
                            body: Box::new(Expr::Subshell(Subshell {
                                expressions: Vec::new()
                            })),
                        },
                        MatchArm {
                            val_name: Some("x"),
                            patterns: vec![
                                MatchPattern::Wildcard
                            ],
                            guard: Some(Expr::Test(Test {
                                expression: Box::new(Expr::Binary(BinaryOperation {
                                    left: Box::new(Expr::VarReference(VarReference {
                                        name: "a"
                                    })),
                                    op: BinaryOperator::EqualEqual,
                                    right: Box::new(Expr::Literal(Literal {
                                        lexeme: "1",
                                        parsed: 1.into(),
                                    })),
                                }))
                            })),
                            body: Box::new(Expr::Subshell(Subshell {
                                expressions: Vec::new()
                            })),
                        },
                        MatchArm {
                            val_name: None,
                            patterns: vec![
                                MatchPattern::Wildcard
                            ],
                            guard: None,
                            body: Box::new(Expr::Call(Call {
                                arguments: vec![
                                    Expr::Literal("echo".into()),
                                    Expr::VarReference(VarReference {
                                        name: "it"
                                    }),
                                ]
                            })),
                        },
                    ],
                })
            ]
        )
    }
}