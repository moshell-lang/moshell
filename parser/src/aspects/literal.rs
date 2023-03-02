use std::num::IntErrorKind;

use crate::aspects::substitution::SubstitutionAspect;
use lexer::token::TokenType;

use crate::ast::literal::{Literal, LiteralValue};
use crate::ast::*;
use crate::err::ParseErrorKind;
use crate::moves::{next, of_type};
use crate::parser::{ParseResult, Parser};
use crate::source::try_join_str;

/// A trait that contains all the methods for parsing literals.
pub(crate) trait LiteralAspect<'a> {
    /// Parses a number-like literal expression.
    fn literal(&mut self) -> ParseResult<Expr<'a>>;

    /// Parses a string literal expression.
    ///
    /// This method is only used for single quoted strings.
    fn string_literal(&mut self) -> ParseResult<Expr<'a>>;

    /// Parses a string template literal expression.
    ///
    /// This method is only used for double quoted strings, which may contain variable references for instance.
    fn templated_string_literal(&mut self) -> ParseResult<Expr<'a>>;

    /// Parse a raw argument.
    ///
    /// Arguments are not quoted and are separated by spaces.
    fn argument(&mut self) -> ParseResult<Expr<'a>>;
    fn parse_literal(&mut self) -> ParseResult<LiteralValue>;
}

impl<'a> LiteralAspect<'a> for Parser<'a> {
    fn literal(&mut self) -> ParseResult<Expr<'a>> {
        Ok(Expr::Literal(Literal {
            lexme: self.cursor.peek().value,
            parsed: self.parse_literal()?,
        }))
    }

    fn string_literal(&mut self) -> ParseResult<Expr<'a>> {
        let start = self
            .cursor
            .force(of_type(TokenType::Quote), "Expected quote.")?;
        let mut lexme = start.value;

        let mut value = String::new();

        loop {
            match self.cursor.next_opt() {
                None => {
                    return self.expected(
                        "Unterminated string literal.",
                        ParseErrorKind::Unpaired(self.cursor.relative_pos(&start)),
                    );
                }

                Some(token) => {
                    if token.token_type == TokenType::Quote {
                        if let Some(joined) = try_join_str(lexme, token.value) {
                            lexme = joined;
                        }
                        break;
                    }
                    value.push_str(token.value);
                    if let Some(joined) = try_join_str(lexme, token.value) {
                        lexme = joined;
                    }
                }
            };
        }
        Ok(Expr::Literal(Literal {
            lexme,
            parsed: LiteralValue::String(value),
        }))
    }

    fn templated_string_literal(&mut self) -> ParseResult<Expr<'a>> {
        let start = self
            .cursor
            .force(of_type(TokenType::DoubleQuote), "Expected quote.")?;
        let mut lexme = self.cursor.peek().value;
        let mut literal_value = String::new();
        let mut parts = Vec::new();
        loop {
            if self.cursor.is_at_end() {
                return self.expected(
                    "Unterminated string literal.",
                    ParseErrorKind::Unpaired(self.cursor.relative_pos(&start)),
                );
            }

            match self.cursor.peek().token_type {
                TokenType::DoubleQuote => {
                    self.cursor.advance(next());
                    break;
                }

                TokenType::Dollar => {
                    if !literal_value.is_empty() {
                        parts.push(Expr::Literal(Literal {
                            lexme,
                            parsed: LiteralValue::String(literal_value.clone()),
                        }));
                        literal_value.clear();
                    }
                    lexme = "";

                    parts.push(self.substitution()?);
                }

                _ => {
                    let value = self.cursor.next()?.value;
                    literal_value.push_str(value);
                    if lexme.is_empty() {
                        lexme = value;
                    } else if let Some(joined) = try_join_str(lexme, value) {
                        lexme = joined;
                    }
                }
            };
        }
        if !literal_value.is_empty() {
            parts.push(Expr::Literal(Literal {
                lexme,
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
        let current = self.cursor.peek();
        let mut parts = Vec::new();
        let mut builder = String::new();
        let mut lexme = current.value;

        //pushes current token then advance
        macro_rules! push_current {
            () => {
                let value = self.cursor.next()?.value;
                builder.push_str(value);
                if lexme.is_empty() {
                    lexme = value;
                } else if let Some(joined) = try_join_str(lexme, value) {
                    lexme = joined;
                }
                ()
            };
        }

        match current.token_type {
            TokenType::At | TokenType::Dollar => parts.push(self.substitution()?),
            TokenType::BackSlash => {
                //never retain first backslash
                self.cursor.next()?; //advance so we are not pointing to token after '\'
                                     //will append the escaped value (token after the backslash)
                push_current!();
            }
            _ => {
                push_current!();
            }
        };
        while !self.cursor.is_at_end() {
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

                TokenType::At | TokenType::Dollar => {
                    if !builder.is_empty() {
                        parts.push(Expr::Literal(Literal {
                            lexme,
                            parsed: LiteralValue::String(builder.clone()),
                        }));
                        builder.clear();
                        lexme = "";
                    }
                    parts.push(self.substitution()?);
                }
                _ if pivot.is_ponctuation() => break,
                _ => {
                    push_current!();
                }
            }
        }
        if !builder.is_empty() {
            parts.push(Expr::Literal(Literal {
                lexme,
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
                |e| match e.kind() {
                    IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => self.mk_parse_error(
                        "Integer constant is too large.".to_string(),
                        token,
                        ParseErrorKind::NotParsable,
                    ),
                    _ => self.mk_parse_error(e.to_string(), token, ParseErrorKind::NotParsable),
                },
            )?)),
            TokenType::FloatLiteral => {
                Ok(LiteralValue::Float(token.value.parse::<f64>().map_err(
                    |e| self.mk_parse_error(e.to_string(), token, ParseErrorKind::NotParsable),
                )?))
            }
            _ => self.expected("Expected a literal.", ParseErrorKind::Unexpected),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse;

    use super::*;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::source::Source;
    use pretty_assertions::assert_eq;

    #[test]
    fn int_overflow() {
        let source = Source::unknown("123456789012345678901234567890");
        let parsed: ParseResult<_> = parse(source).into();
        assert_eq!(
            parsed,
            Err(ParseError {
                message: "Integer constant is too large.".to_string(),
                position: 0..30,
                kind: ParseErrorKind::NotParsable,
            })
        );
    }

    #[test]
    fn string_literal() {
        let source = Source::unknown("'hello $world! $(this is a test) @(of course)'");
        let parsed = Parser::new(source).expression().expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::Literal(Literal {
                lexme: "'hello $world! $(this is a test) @(of course)'",
                parsed: "hello $world! $(this is a test) @(of course)".into(),
            })
        );
    }

    #[test]
    fn escaped_literal() {
        let source = Source::unknown("a\\a");
        let parsed = Parser::new(source).expression().expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::Literal(Literal {
                lexme: "a",
                parsed: "aa".into(),
            })
        );
    }

    #[test]
    fn missing_quote() {
        let content = "' command";
        let source = Source::unknown(content);
        let parsed: ParseResult<_> = parse(source).into();
        assert_eq!(
            parsed,
            Err(ParseError {
                message: "Unterminated string literal.".to_string(),
                position: content.len() - 1..content.len(),
                kind: ParseErrorKind::Unpaired(0..1),
            })
        );
    }
}
