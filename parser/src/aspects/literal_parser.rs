use std::num::IntErrorKind;

use lexer::token::TokenType;

use crate::aspects::var_reference_parser::VarReferenceParser;
use crate::ast::literal::{Literal, LiteralValue};
use crate::ast::*;
use crate::moves::{escapable, MoveOperations, next, of_type, space};
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
            token: self.cursor.peek().clone(),
            parsed: self.parse_literal()?,
        }))
    }

    fn string_literal(&mut self) -> ParseResult<Expr<'a>> {
        let token = self
            .cursor
            .force(of_type(TokenType::Quote), "Expected quote.")?;

        let mut value = String::new();

        loop {
            match self.cursor.next_opt() {
                None => {
                    return self.expected("Unterminated string literal.");
                }

                Some(token) => {
                    if token.token_type == TokenType::Quote {
                        break;
                    }
                    value.push_str(token.value);
                }
            };
        }
        Ok(Expr::Literal(Literal {
            token,
            parsed: LiteralValue::String(value),
        }))
    }

    fn templated_string_literal(&mut self) -> ParseResult<Expr<'a>> {
        self.cursor
            .force(of_type(TokenType::DoubleQuote), "Expected quote.")?;
        let mut current_start = self.cursor.peek();
        let mut literal_value = String::new();
        let mut parts = Vec::new();
        loop {
            if self.cursor.is_at_end() {
                return self.expected("Unterminated string literal.");
            }

            match self.cursor.peek().token_type {
                TokenType::DoubleQuote => {
                    self.cursor.advance(next());
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
                    current_start = self.cursor.peek();
                }

                _ => literal_value.push_str(self.cursor.next()?.value),
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
        let mut current = self.cursor.peek();
        let mut parts = Vec::new();
        let mut builder = String::new();


        //pushes current token then advance
        macro_rules! push_current {
            () => { builder.push_str(self.cursor.next()?.value) };
        }

        match current.token_type {
            TokenType::Dollar => parts.push(self.var_reference()?),
            TokenType::BackSlash => {
                //never retain first backslash
                self.cursor.next()?; //advance so we are not pointing to token after '\'
                //will append the escaped value (token after the backslash)
                push_current!();
            }
            _ => push_current!(),
        }
        loop {
            if self.cursor.is_at_end() {
                break;
            }
            let pivot = self.cursor.peek().token_type;
            match pivot {
                TokenType::Space => break,

                TokenType::BackSlash => {
                    //never retain first backslash
                    self.cursor.next()?;
                    //advance so we are not pointing to token after '\'
                    //will append the escaped value (token after the backslash)
                    push_current!();
                }

                TokenType::Dollar => {
                    if !builder.is_empty() {
                        parts.push(Expr::Literal(Literal {
                            token: current.clone(),
                            parsed: LiteralValue::String(builder.clone()),
                        }));
                        builder.clear();
                    }
                    parts.push(self.var_reference()?);
                }

                x => {
                    if x.is_identifier_bound() {
                        break; //identifier bounds cannot be concatenated with identifiers
                    }
                    if !builder.is_empty() {
                        current = self.cursor.peek();
                    }
                    push_current!()
                }
            }
        }
        if !builder.is_empty() {
            parts.push(Expr::Literal(Literal {
                token: current,
                parsed: LiteralValue::String(builder),
            }));
        }
        if parts.len() == 1 {
            return Ok(parts.pop().unwrap());
        }
        Ok(Expr::TemplateString(parts))
    }

    fn parse_literal(&mut self) -> ParseResult<LiteralValue> {
        let token = self.cursor.next()?;
        match token.token_type {
            TokenType::IntLiteral => Ok(LiteralValue::Int(token.value.parse::<i64>().map_err(
                |e| {
                    match e.kind() {
                        IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => self
                            .expected::<()>("Integer constant is too large.")
                            .unwrap_err(),
                        _ => self.mk_parse_error(e.to_string()),
                    }
                },
            )?)),
            TokenType::FloatLiteral => Ok(LiteralValue::Float(
                token
                    .value
                    .parse::<f64>()
                    .map_err(|e| self.mk_parse_error(e.to_string()))?,
            )),
            _ => self.expected("Expected a literal."),
        }
    }
}

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
    use lexer::token::Token;

    use crate::parse;
    use crate::parser::ParseError;

    use super::*;
    use pretty_assertions::assert_eq;
    use crate::ast::callable::Call;

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
        let tokens = lex("'hello $world! $(this is a test) @(of course)'");
        let parsed = Parser::new(tokens).expression().expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::Literal(Literal {
                token: Token::new(TokenType::Quote, "'"),
                parsed: LiteralValue::String(
                    "hello $world! $(this is a test) @(of course)".to_string()
                ),
            })
        );
    }

    #[test]
    fn escaped_literal() {
        let tokens = lex("a\\a");
        let parsed = Parser::new(tokens).expression().expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::Literal(Literal {
                token: Token::new(TokenType::Identifier, "a"),
                parsed: LiteralValue::String("aa".to_string()),
            })
        );
    }



    #[test]
    fn missing_quote() {
        let tokens = vec![Token::new(TokenType::Quote, "' command")];
        let parsed = parse(tokens);
        assert_eq!(
            parsed,
            Err(ParseError {
                message: "Unterminated string literal.".to_string(),
            })
        );
    }

}
