use lexer::token::TokenType::{And, Or, CurlyRightBracket, RoundedRightBracket};
use crate::aspects::redirection_parser::RedirectionParser;
use crate::ast::callable::Call;
use crate::ast::Expr;
use crate::context::ParserContext;
use crate::moves::{custom_eox, of_types, space, spaces, MoveOperations};
use crate::parser::{Parser, ParseResult};

/// A parse aspect for command and function calls
pub trait CallParser<'a> {
    /// Attempts to parse the next call expression
    fn call(&mut self) -> ParseResult<Expr<'a>>;

}

/// The end of a call expression
impl<'a> CallParser<'a> for Parser<'a> {
    fn call(&mut self) -> ParseResult<Expr<'a>> {

        let mut arguments = vec![self.expression(ParserContext::default())?];
        // tests if this cursor hits caller-defined eoc or [And, Or] tokens
        macro_rules! eoc_hit { () => {
            self.cursor.lookahead(spaces().then(custom_eox(of_types(&[And, Or, CurlyRightBracket, RoundedRightBracket])))).is_some() };
        }

        while !self.cursor.is_at_end() && !eoc_hit!() {
            self.cursor.advance(space()); //consume spaces

            if self.is_at_redirection_sign() {
                return self.redirectable(Expr::Call(Call { arguments }));
            }
            arguments.push(self.expression(ParserContext::default())?);
        }
        Ok(Expr::Call(Call { arguments }))
    }


}

#[cfg(test)]
mod tests {
    use crate::ast::callable::{Call};
    use crate::ast::literal::Literal;
    use crate::ast::Expr;
    use crate::parse;
    use lexer::lexer::lex;
    use lexer::token::{Token, TokenType};
    use pretty_assertions::assert_eq;


    #[test]
    fn multiple_calls() {
        let tokens = lex("grep -E regex; echo test");
        let parsed = parse(tokens).expect("parsing error");
        assert_eq!(
            parsed,
            vec![
                Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "grep"),
                            parsed: "grep".into(),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "E"),
                            parsed: "-E".into(),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "regex"),
                            parsed: "regex".into(),
                        }),
                    ],
                }),
                Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "echo"),
                            parsed: "echo".into(),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "test"),
                            parsed: "test".into(),
                        }),
                    ],
                }),
            ]
        )
    }


}
