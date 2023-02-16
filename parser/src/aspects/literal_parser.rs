use std::num::IntErrorKind;

use lexer::token::TokenType;

use crate::aspects::base_parser::BaseParser;
use crate::aspects::var_reference_parser::VarReferenceParser;
use crate::ast::literal::{Literal, LiteralValue};
use crate::ast::*;
use crate::parser::{ParseResult, Parser};

pub(crate) trait LiteralParser<'a> {
    fn literal(&mut self) -> ParseResult<Expr<'a>>;
    fn string_literal(&mut self) -> ParseResult<Expr<'a>>;
    fn templated_string_literal(&mut self) -> ParseResult<Expr<'a>>;
    fn argument(&mut self) -> ParseResult<Expr<'a>>;
    fn parse_literal(&mut self) -> ParseResult<LiteralValue>;
}

impl<'a> LiteralParser<'a> for Parser<'a> {
    fn literal(&mut self) -> ParseResult<Expr<'a>> {
        Ok(Expr::Literal(Literal {
            token: self.cursor().peek_token().clone(),
            parsed: self.parse_literal()?,
        }))
    }

    fn string_literal(&mut self) -> ParseResult<Expr<'a>> {
        let cursor = self.cursor();
        let token = cursor.next_token()?;
        let mut value = String::new();
        loop {
            if cursor.is_at_end() {
                return cursor.expected("Unterminated string literal.");
            }

            let token = self.next_token_space_aware()?;
            if token.token_type == TokenType::Quote {
                break;
            }
            value.push_str(token.value);
        }
        Ok(Expr::Literal(Literal {
            token: token.clone(),
            parsed: LiteralValue::String(value),
        }))
    }

    fn templated_string_literal(&mut self) -> ParseResult<Expr<'a>> {
        self.next_token()?;
        let mut current_start = self.peek_token_space_aware();
        let mut literal_value = String::new();
        let mut parts = Vec::new();
        loop {
            if self.is_at_end() {
                return Err(self.mk_parse_error("Unterminated string literal."));
            }
            match self.peek_token_space_aware().token_type {
                TokenType::DoubleQuote => {
                    self.next_token()?;
                    break;
                }
                TokenType::Dollar => {
                    if !literal_value.is_empty() {
                        parts.push(Expr::Literal(Literal {
                            token: current_start,
                            parsed: LiteralValue::String(literal_value.clone()),
                        }));
                        literal_value.clear();
                    }
                    let var_ref = self.var_reference()?;
                    parts.push(var_ref);
                    current_start = self.peek_token_space_aware();
                }
                _ => literal_value.push_str(self.next_token_space_aware()?.value),
            };
        }
        if !literal_value.is_empty() {
            parts.push(Expr::Literal(Literal {
                token: current_start,
                parsed: LiteralValue::String(literal_value),
            }));
        }
        Ok(Expr::TemplateString(parts))
    }

    /// Parses a single argument.
    ///
    /// An argument is usually a single identifier, but can also be
    /// composed of multiple tokens if not separated with a space.
    fn argument(&mut self) -> ParseResult<Expr<'a>> {
        let mut current_start = self.peek_token();
        let mut parts = Vec::new();
        let mut builder = String::new();
        match current_start.token_type {
            TokenType::Dollar => parts.push(self.var_reference()?),
            _ => builder.push_str(self.next_token()?.value),
        }
        loop {
            if self.is_at_end() {
                break;
            }
            match self.peek_token_space_aware().token_type {
                TokenType::Space => {
                    self.next_token_space_aware()?;
                    break;
                }
                TokenType::Dollar => {
                    if !builder.is_empty() {
                        parts.push(Expr::Literal(Literal {
                            token: current_start.clone(),
                            parsed: LiteralValue::String(builder.clone()),
                        }));
                        builder.clear();
                    }
                    parts.push(self.var_reference()?);
                }
                _ => {
                    if !builder.is_empty() {
                        current_start = self.peek_token_space_aware();
                    }
                    builder.push_str(self.next_token_space_aware()?.value)
                }
            }
        }
        if !builder.is_empty() {
            parts.push(Expr::Literal(Literal {
                token: current_start,
                parsed: LiteralValue::String(builder),
            }));
        }
        if parts.len() == 1 {
            return Ok(parts.pop().unwrap());
        }
        Ok(Expr::TemplateString(parts))
    }

    fn parse_literal(&mut self) -> ParseResult<LiteralValue> {
        let cursor = self.cursor();

        let token = cursor.next_token()?;
        match token.token_type {
            TokenType::IntLiteral => Ok(LiteralValue::Int(token.value.parse::<i64>().map_err(
                |e| match e.kind() {
                    IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                        self.cursor().expected("Integer constant is too large.")
                    }
                    _ => self.cursor().expected(&e.to_string()),
                },
            )?)),
            TokenType::FloatLiteral => Ok(LiteralValue::Float(
                token
                    .value
                    .parse::<f64>()
                    .map_err(|e| cursor.expected(&e.to_string()))?,
            )),
            _ => cursor.expected("Expected a literal."),
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
        let parsed = Parser::new(tokens).expression().expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::Literal(Literal {
                token: Token::new(TokenType::Quote, "'"),
                parsed: LiteralValue::String("hello world!".to_string()),
            })
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
