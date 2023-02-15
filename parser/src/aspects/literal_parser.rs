use std::num::IntErrorKind;

use lexer::token::TokenType;

use crate::aspects::base_parser::BaseParser;
use crate::ast::literal::{Literal, LiteralValue};
use crate::ast::*;
use crate::parser::{ParseResult, Parser};

pub(crate) trait LiteralParser<'a> {
    fn literal(&mut self) -> ParseResult<Expr<'a>>;
    fn string_literal(&mut self) -> ParseResult<Expr<'a>>;
    fn argument(&mut self) -> ParseResult<Expr<'a>>;
    fn parse_literal(&mut self) -> ParseResult<LiteralValue>;
}

impl<'a> LiteralParser<'a> for Parser<'a> {
    fn literal(&mut self) -> ParseResult<Expr<'a>> {
        Ok(Expr::Literal(Literal {
            token: self.peek_token(),
            parsed: self.parse_literal()?,
        }))
    }

    fn string_literal(&mut self) -> ParseResult<Expr<'a>> {
        let token = self.next_token()?;
        let mut value = String::new();
        loop {
            if self.is_at_end() {
                return Err(self.mk_parse_error("Unterminated string literal."));
            }
            let token = self.next_token_space_aware()?;
            if token.token_type == TokenType::Quote {
                break;
            }
            value.push_str(token.value);
        }
        Ok(Expr::Literal(Literal {
            token,
            parsed: LiteralValue::String(value),
        }))
    }

    fn argument(&mut self) -> ParseResult<Expr<'a>> {
        let token = self.next_token()?;
        let mut value = token.value.to_string();
        loop {
            if self.is_at_end() {
                break;
            }
            let token = self.next_token_space_aware()?;
            if token.token_type == TokenType::Space {
                break;
            }
            value.push_str(token.value);
        }
        Ok(Expr::Literal(Literal {
            token,
            parsed: LiteralValue::String(value),
        }))
    }

    fn parse_literal(&mut self) -> ParseResult<LiteralValue> {
        let token = self.next_token()?;
        match token.token_type {
            TokenType::IntLiteral => Ok(LiteralValue::Int(token.value.parse::<i64>().map_err(
                |e| match e.kind() {
                    IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                        self.mk_parse_error("Integer constant is too large.")
                    }
                    _ => self.mk_parse_error(e.to_string()),
                },
            )?)),
            TokenType::FloatLiteral => Ok(LiteralValue::Float(
                token
                    .value
                    .parse::<f64>()
                    .map_err(|e| self.mk_parse_error(e.to_string()))?,
            )),
            _ => Err(self.mk_parse_error("Expected a literal.")),
        }
    }
}

#[cfg(test)]
mod tests {
    use lexer::token::Token;

    use crate::parse;
    use crate::parser::ParseError;

    use super::*;

    #[test]
    fn int_overflow() {
        let tokens = vec![Token::new(
            TokenType::IntLiteral,
            "123456789012345678901234567890",
        )];
        let parsed = parse(tokens);
        assert_eq!(
            parsed,
            Err(ParseError {
                message: "Integer constant is too large.".to_string(),
            })
        );
    }

    #[test]
    fn string_literal() {
        let tokens = vec![
            Token::new(TokenType::Quote, "'"),
            Token::new(TokenType::Identifier, "hello"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Identifier, "world"),
            Token::new(TokenType::Not, "!"),
            Token::new(TokenType::Quote, "'"),
        ];
        let parsed = parse(tokens).expect("Failed to parse.");
        assert_eq!(
            parsed,
            vec![Expr::Literal(Literal {
                token: Token::new(TokenType::Quote, "'"),
                parsed: LiteralValue::String("hello world!".to_string()),
            })]
        );
    }

    #[test]
    fn missing_quote() {
        let tokens = vec![Token::new(TokenType::Quote, "'")];
        let parsed = parse(tokens);
        assert_eq!(
            parsed,
            Err(ParseError {
                message: "Unterminated string literal.".to_string(),
            })
        );
    }
}
