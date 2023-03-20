use crate::aspects::call::CallAspect;
use crate::err::ParseErrorKind;
use crate::moves::{of_type, spaces, times, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::test::{Not, Test};
use ast::Expr;
use ast::Expr::Literal;
use lexer::token::TokenType::{SquaredLeftBracket, SquaredRightBracket};
use lexer::token::{Token, TokenType};

pub(crate) trait TestAspect<'a> {
    ///parse a not (! ..) expression.
    fn not<P>(&mut self, parse_next: P) -> ParseResult<Expr<'a>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'a>>;

    ///parse [[ ... ]] or [ .. ] expression.
    fn parse_test(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> TestAspect<'a> for Parser<'a> {
    fn not<P>(&mut self, mut parse_next: P) -> ParseResult<Expr<'a>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'a>>,
    {
        self.cursor.force(of_type(TokenType::Not), "expected '!'")?;

        Ok(Expr::Not(Not {
            underlying: Box::new(parse_next(self)?),
        }))
    }

    fn parse_test(&mut self) -> ParseResult<Expr<'a>> {
        //expect the first '[' lexeme

        let start = self.cursor.force(
            of_type(SquaredLeftBracket),
            "expected '[' at start of test expression.",
        )?;

        //if first bracket is followed by a second, then this expression is a direct call to the `test` command.
        if self.cursor.advance(of_type(SquaredLeftBracket)).is_some() {
            return self.parse_call(start);
        }

        if let Some(end) = self.cursor.lookahead(of_type(SquaredRightBracket)) {
            self.expected_with(
                "native test evaluation cannot be empty.",
                start.clone()..end,
                ParseErrorKind::Unexpected,
            )?;
        }

        let underlying = Box::new(self.value()?);
        self.cursor.force_with(
            //expect trailing ']'
            spaces().then(of_type(SquaredRightBracket)),
            "missing ']'",
            ParseErrorKind::Unpaired(self.cursor.relative_pos(&start)),
        )?;
        Ok(Expr::Test(Test {
            expression: underlying,
        }))
    }
}

impl<'a> Parser<'a> {
    fn parse_call(&mut self, start: Token) -> ParseResult<Expr<'a>> {
        let call = self.call_arguments(Literal("test".into()), Vec::new());

        self.cursor.force_with(
            //expect trailing ']]'
            spaces().then(times(2, of_type(SquaredRightBracket))),
            "missing ']]'",
            ParseErrorKind::Unpaired(self.cursor.relative_pos(start)),
        )?;
        call
    }
}

#[cfg(test)]
mod tests {
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::ParseResult;
    use ast::call::Call;
    use ast::group::{Parenthesis, Subshell};
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::test::{Not, Test};
    use ast::value::{Literal, LiteralValue};
    use ast::variable::VarReference;
    use ast::Expr;
    use context::source::Source;
    use pretty_assertions::assert_eq;

    #[test]
    fn native_empty() {
        let content = "[]";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "native test evaluation cannot be empty.".to_string(),
                position: 0..content.len(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn native_empty_not() {
        let content = "[! ]";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Unexpected token ']'.".to_string(),
                position: content.len() - 1..content.len(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn call_empty() {
        let source = Source::unknown("[[]]");
        let result = parse(source).expect("parsing failed");
        assert_eq!(
            result,
            vec![Expr::Call(Call {
                arguments: vec![Expr::Literal("test".into())],
                type_parameters: vec![],
            })]
        )
    }

    #[test]
    fn call_with_content() {
        let source = Source::unknown("[[48 -gt 100]]");
        let result = parse(source).expect("parsing failed");
        assert_eq!(
            result,
            vec![Expr::Call(Call {
                arguments: vec![
                    Expr::Literal("test".into()),
                    Expr::Literal(Literal {
                        lexeme: "48",
                        parsed: LiteralValue::Int(48),
                    }),
                    Expr::Literal("-gt".into()),
                    Expr::Literal(Literal {
                        lexeme: "100",
                        parsed: LiteralValue::Int(100),
                    }),
                ],
                type_parameters: vec![],
            })]
        )
    }

    #[test]
    fn integration() {
        let source = Source::unknown("echo && [ ($a == $b) ] || [[ $1 ]]");
        let result = parse(source).expect("parse error");
        assert_eq!(
            result,
            vec![Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Call(Call {
                        arguments: vec![Expr::Literal("echo".into())],
                        type_parameters: vec![],
                    })),
                    op: BinaryOperator::And,
                    right: Box::new(Expr::Test(Test {
                        expression: Box::new(Expr::Parenthesis(Parenthesis {
                            expression: Box::new(Expr::Binary(BinaryOperation {
                                left: Box::new(Expr::VarReference(VarReference { name: "a" })),
                                op: BinaryOperator::EqualEqual,
                                right: Box::new(Expr::VarReference(VarReference { name: "b" })),
                            }))
                        })),
                    }))
                })),
                op: BinaryOperator::Or,
                right: Box::new(Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal("test".into()),
                        Expr::VarReference(VarReference { name: "1" }),
                    ],
                    type_parameters: vec![],
                })),
            })]
        )
    }

    #[test]
    fn in_test() {
        let content = "[ test == [] ]";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Unexpected start of test expression".to_string(),
                position: content[1..].find('[').map(|p| p + 1..p + 2).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn unclosed_test() {
        let content = "[ test == $USER ";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "missing ']'".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unpaired(0..1),
            })
        )
    }

    #[test]
    fn unclosed_call() {
        let content = "[[ test == $USER ";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "missing ']]'".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unpaired(0..1),
            })
        )
    }

    #[test]
    fn not_call() {
        let source = Source::unknown("!grep -E '^[0-9]+$'");
        let result = parse(source).expect("parse fail");
        assert_eq!(
            result,
            vec![Expr::Not(Not {
                underlying: Box::new(Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal("grep".into()),
                        Expr::Literal("-E".into()),
                        Expr::Literal(Literal {
                            lexeme: "'^[0-9]+$'",
                            parsed: LiteralValue::String("^[0-9]+$".to_string()),
                        }),
                    ],
                    type_parameters: vec![],
                }))
            })]
        )
    }

    #[test]
    fn not() {
        let source = Source::unknown("! ($a && $b) || ! $2 == 78");
        let result = parse(source).expect("parse error");
        assert_eq!(
            result,
            vec![Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Not(Not {
                    underlying: Box::new(Expr::Subshell(Subshell {
                        expressions: vec![Expr::Binary(BinaryOperation {
                            left: Box::new(Expr::VarReference(VarReference { name: "a" })),
                            op: BinaryOperator::And,
                            right: Box::new(Expr::VarReference(VarReference { name: "b" })),
                        })]
                    }))
                })),
                op: BinaryOperator::Or,
                right: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Not(Not {
                        underlying: Box::new(Expr::VarReference(VarReference { name: "2" }))
                    })),
                    op: BinaryOperator::EqualEqual,
                    right: Box::new(Expr::Literal(Literal {
                        lexeme: "78",
                        parsed: 78.into(),
                    })),
                })),
            })]
        )
    }
}
