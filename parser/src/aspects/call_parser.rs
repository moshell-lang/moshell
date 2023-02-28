use lexer::token::TokenType::{And, Or, CurlyRightBracket, RoundedRightBracket, SquaredRightBracket};
use crate::aspects::redirection_parser::RedirectionParser;
use crate::ast::callable::Call;
use crate::ast::Expr;
use crate::moves::{unescaped, of_types, space, spaces, MoveOperations};
use crate::parser::{Parser, ParseResult};

/// A parse aspect for command and function calls
pub trait CallParser<'a> {
    /// Attempts to parse the next call expression
    fn call(&mut self) -> ParseResult<Expr<'a>>;

}

impl<'a> CallParser<'a> for Parser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>> {

        let mut arguments = vec![self.expression()?];
        // tests if this cursor hits caller-defined eoc or [And, Or] tokens
        macro_rules! eoc_hit { () => {
            self.cursor.lookahead(spaces().then(unescaped(of_types(&[And, Or, CurlyRightBracket, RoundedRightBracket, SquaredRightBracket])))).is_some() };
        }

        while !self.cursor.is_at_end() && !eoc_hit!() {
            self.cursor.advance(space()); //consume spaces

            if self.is_at_redirection_sign() {
                return self.redirectable(Expr::Call(Call { arguments }));
            }
            arguments.push(self.expression()?);
        }
        Ok(Expr::Call(Call { arguments }))
    }


}

#[cfg(test)]
mod tests {
    use crate::ast::Expr;
    use crate::parse;
    use lexer::lexer::lex;
    use pretty_assertions::assert_eq;
    use crate::ast::callable::Call;
    use crate::ast::literal::Literal;
    use crate::parser::ParseError;

    #[test]
    fn wrong_group_end() {
        let tokens = lex("ls )");
        assert_eq!(
            parse(tokens),
            Err(ParseError {
                message: "invalid token".to_string()
            })
        );
    }

    #[test]
    fn multiple_calls() {
        let tokens = lex("grep -E regex; echo test");
        let parsed = parse(tokens).expect("parsing error");
        assert_eq!(
            parsed,
            vec![
                Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal("grep".into()),
                        Expr::Literal("-E".into()),
                        Expr::Literal("regex".into()),
                    ],
                }),
                Expr::Call(Call {
                    arguments: vec![Expr::Literal("echo".into()), Expr::Literal("test".into())],
                }),
            ]
        )
    }


    #[test]
    fn escaped_call() {
        let tokens = lex("grep -E regex \\; echo test");
        let parsed = parse(tokens).expect("parsing error");
        assert_eq!(
            parsed,
            vec![
                Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal("grep".into()),
                        Expr::Literal("-E".into()),
                        Expr::Literal("regex".into()),
                        Expr::Literal(Literal {
                            lexme: "\\;",
                            parsed: ";".into(),
                        }),
                        Expr::Literal("echo".into()),
                        Expr::Literal("test".into()),
                    ],
                }),
            ]
        )
    }


}
