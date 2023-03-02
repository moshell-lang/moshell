use lexer::token::TokenType;
use lexer::token::TokenType::{SquaredRightBracket, SquaredLeftBracket};
use crate::aspects::call::CallAspect;
use crate::ast::Expr;
use crate::ast::Expr::Literal;
use crate::ast::test::{Not, Test};
use crate::moves::{MoveOperations, of_type, spaces, times};
use crate::parser::{Parser, ParseResult};

pub(crate) trait TestAspect<'a> {

    ///parse a not (! ..) expression.
    fn not<P>(&mut self, parse_next: P) -> ParseResult<Expr<'a>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>>;

    ///parse [[ ... ]] or [ .. ] expression.
    fn parse_test(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> TestAspect<'a> for Parser<'a> {
    fn not<P>(&mut self, mut parse_next: P) -> ParseResult<Expr<'a>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>>
    {
        self.cursor.force(
            of_type(TokenType::Not),
            "expected '!'",
        )?;

        Ok(Expr::Not(Not { right: Box::new(parse_next(self)?) }))
    }

    fn parse_test(&mut self) -> ParseResult<Expr<'a>> {
        //expect the first '[' lexeme
        self.cursor.force(of_type(SquaredLeftBracket), "expected '[' at start of test expression.")?;

        //if first bracket is followed by a second, then this expression is a direct call to the `test` command.
        if self.cursor.advance(of_type(SquaredLeftBracket)).is_some() {
            return self.parse_test_call();
        }

        if self.cursor.lookahead(of_type(SquaredRightBracket)).is_some() {
            self.expected("native test evaluation cannot be empty.")?
        }

        let underlying = Box::new(self.value()?);
        self.cursor.force( //expect trailing ']'
                           spaces().then(of_type(SquaredRightBracket)),
                           "missing ']'")?;
        Ok(Expr::Test(Test { expression: underlying }))
    }
}

impl<'a> Parser<'a> {
    fn parse_test_call(&mut self) -> ParseResult<Expr<'a>> {
        let call = self.call_arguments(Literal("test".into()));
        self.cursor.force( //expect trailing ']]'
                           spaces().then(times(2, of_type(SquaredRightBracket))),
                           "missing ']]'")?;
        call
    }
}

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
    use crate::parse;
    use pretty_assertions::assert_eq;
    use crate::ast::callable::Call;
    use crate::ast::Expr;
    use crate::ast::group::{Parenthesis, Subshell};
    use crate::ast::literal::{Literal, LiteralValue};
    use crate::ast::operation::{BinaryOperation, BinaryOperator};
    use crate::ast::test::{Not, Test};
    use crate::ast::variable::VarReference;
    use crate::parser::ParseError;


    #[test]
    fn native_empty() {
        let result = parse(lex("[]"));
        assert_eq!(
            result,
            Err(ParseError {
                message: "native test evaluation cannot be empty.".to_string()
            })
        )
    }

    #[test]
    fn native_empty_not() {
        let result = parse(lex("[! ]"));
        assert_eq!(
            result,
            Err(ParseError {
                message: "Unexpected closing bracket.".to_string()
            })
        )
    }

    #[test]
    fn call_empty() {
        let result = parse(lex("[[]]")).expect("parsing failed");
        assert_eq!(
            result,
            vec![
                Expr::Call(Call {
                    arguments: vec![Expr::Literal("test".into())]
                })
            ]
        )
    }

    #[test]
    fn call_with_content() {
        let result = parse(lex("[[48 -gt 100]]")).expect("parsing failed");
        assert_eq!(
            result,
            vec![
                Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal("test".into()),
                        Expr::Literal(Literal {
                            lexme: "48",
                            parsed: LiteralValue::Int(48),
                        }),
                        Expr::Literal("-gt".into()),
                        Expr::Literal(Literal {
                            lexme: "100",
                            parsed: LiteralValue::Int(100),
                        }),
                    ]
                })
            ]
        )
    }

    #[test]
    fn test_integration() {
        let result = parse(lex("echo && [ ($a == $b) ] || [[ $1 ]]")).expect("parse error");
        assert_eq!(
            result,
            vec![
                Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Call(Call {
                            arguments: vec![Expr::Literal("echo".into())]
                        })),
                        op: BinaryOperator::And,
                        right: Box::new(Expr::Test(Test {
                            expression: Box::new(Expr::Parenthesis(Parenthesis {
                                expression: Box::new(Expr::Binary(BinaryOperation {
                                    left: Box::new(Expr::VarReference(VarReference {
                                        name: "a"
                                    })),
                                    op: BinaryOperator::EqualEqual,
                                    right: Box::new(Expr::VarReference(VarReference {
                                        name: "b"
                                    })),
                                }))
                            }))
                            ,
                        }))
                    })),
                    op: BinaryOperator::Or,
                    right: Box::new(Expr::Call(Call
                    {
                        arguments: vec![
                            Expr::Literal("test".into()),
                            Expr::VarReference(VarReference {
                                name: "1"
                            }),
                        ]
                    })),
                })
            ]
        )
    }

    #[test]
    fn test_in_test() {
        let result = parse(lex("[ test == [] ]"));
        assert_eq!(
            result,
            Err(ParseError {
                message: "Unexpected start of test expression".to_string()
            })
        )
    }

    #[test]
    fn unclosed_test() {
        let result = parse(lex("[ test == $USER "));
        assert_eq!(
            result,
            Err(ParseError {
                message: "missing ']'".to_string()
            })
        )
    }

    #[test]
    fn unclosed_test_call() {
        let result = parse(lex("[[ test == $USER "));
        assert_eq!(
            result,
            Err(ParseError {
                message: "missing ']]'".to_string()
            })
        )
    }

    #[test]
    fn not_call() {
        let result = parse(lex("!grep -E '^[0-9]+$'")).expect("parse fail");
        assert_eq!(
            result,
            vec![
                Expr::Not(Not {
                    right: Box::new(Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal("grep".into()),
                            Expr::Literal("-E".into()),
                            Expr::Literal(Literal {
                                lexme: "'^[0-9]+$'",
                                parsed: LiteralValue::String("^[0-9]+$".to_string())
                            })
                        ]
                    }))
                })
            ]
        )

    }

    #[test]
    fn not() {
        let result = parse(lex("! ($a && $b) || ! $2 == 78")).expect("parse error");
        assert_eq!(
            result,
            vec![
                Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Not(Not {
                        right: Box::new(Expr::Subshell(Subshell {
                            expressions: vec![
                                Expr::Binary(BinaryOperation {
                                    left: Box::new(Expr::VarReference(VarReference {
                                        name: "a"
                                    })),
                                    op: BinaryOperator::And,
                                    right: Box::new(Expr::VarReference(VarReference {
                                        name: "b"
                                    })),
                                })
                            ]
                        }))
                    })),
                    op: BinaryOperator::Or,
                    right: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Not(Not {
                            right: Box::new(Expr::VarReference(VarReference {
                                name: "2"
                            }))
                        })),
                        op: BinaryOperator::EqualEqual,
                        right: Box::new(Expr::Literal(Literal {
                            lexme: "78",
                            parsed: 78.into(),
                        })),
                    })),
                })
            ]
        )
    }
}