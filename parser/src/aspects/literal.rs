use std::num::IntErrorKind;

use crate::aspects::substitution::SubstitutionAspect;
use lexer::token::TokenType::*;

use crate::ast::value::{Literal, LiteralValue, TemplateString};
use crate::ast::*;
use crate::moves::{next, of_type, word_sep};
use crate::parser::{ParseResult, Parser};
use crate::source::try_join_str;

/// A trait that contains all the methods for parsing literals.
pub(crate) trait LiteralAspect<'a> {

    /// Parses any literal, number, argument, string, string template
    fn literal(&mut self) -> ParseResult<Expr<'a>>;

    /// Parses a number-like literal expression.
    fn number_literal(&mut self) -> ParseResult<Literal<'a>>;

    /// Parses a string literal expression.
    ///
    /// This method is only used for single quoted strings.
    fn string_literal(&mut self) -> ParseResult<Literal<'a>>;

    /// Parses a string template literal expression.
    ///
    /// This method is only used for double quoted strings, which may contain variable references for instance.
    fn templated_string_literal(&mut self) -> ParseResult<TemplateString<'a>>;

    /// Parse a raw argument.
    ///
    /// Arguments are not quoted and are separated by spaces.
    fn argument(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> LiteralAspect<'a> for Parser<'a> {
    fn literal(&mut self) -> ParseResult<Expr<'a>> {
        let token = self.cursor.peek();
        let pivot = token.token_type;
        match pivot {
            IntLiteral | FloatLiteral => self.number_literal().map(Expr::Literal),
            Quote => self.string_literal().map(Expr::Literal),
            DoubleQuote => self.templated_string_literal().map(Expr::TemplateString),

            _ if pivot.is_keyword() => self.expected(&format!("Unexpected keyword '{}'", token.value)),
            _ if pivot.is_ponctuation() => self.expected(&format!("Unexpected token '{}'.", token.value)),

            _ => self.argument(),
        }
    }

    fn number_literal(&mut self) -> ParseResult<Literal<'a>> {
        Ok(Literal {
            lexeme: self.cursor.peek().value,
            parsed: self.parse_number_value()?,
        })
    }

    fn string_literal(&mut self) -> ParseResult<Literal<'a>> {
        let mut lexeme = self
            .cursor
            .force(of_type(Quote), "Expected quote.")?
            .value;

        let mut value = String::new();

        loop {
            match self.cursor.next_opt() {
                None => {
                    return self.expected("Unterminated string literal.");
                }

                Some(token) => {
                    if token.token_type == Quote {
                        if let Some(joined) = try_join_str(lexeme, token.value) {
                            lexeme = joined;
                        }
                        break;
                    }
                    value.push_str(token.value);
                    if let Some(joined) = try_join_str(lexeme, token.value) {
                        lexeme = joined;
                    }
                }
            };
        }
        Ok(Literal {
            lexeme,
            parsed: LiteralValue::String(value),
        })
    }

    fn templated_string_literal(&mut self) -> ParseResult<TemplateString<'a>> {
        self.cursor
            .force(of_type(DoubleQuote), "Expected quote.")?;
        let mut lexeme = self.cursor.peek().value;
        let mut literal_value = String::new();
        let mut parts = Vec::new();
        loop {
            if self.cursor.is_at_end() {
                return self.expected("Unterminated string literal.");
            }

            match self.cursor.peek().token_type {
                DoubleQuote => {
                    self.cursor.advance(next());
                    break;
                }

                Dollar => {
                    if !literal_value.is_empty() {
                        parts.push(Expr::Literal(Literal {
                            lexeme,
                            parsed: LiteralValue::String(literal_value.clone()),
                        }));
                        literal_value.clear();
                    }
                    lexeme = "";

                    parts.push(self.substitution()?);
                }

                _ => {
                    let value = self.cursor.next()?.value;
                    literal_value.push_str(value);
                    if lexeme.is_empty() {
                        lexeme = value;
                    } else if let Some(joined) = try_join_str(lexeme, value) {
                        lexeme = joined;
                    }
                }
            };
        }
        if !literal_value.is_empty() {
            parts.push(Expr::Literal(Literal {
                lexeme,
                parsed: LiteralValue::String(literal_value),
            }));
        }

        Ok(TemplateString {
            parts
        })
    }

    /// Parses a single argument.
    ///
    /// An argument is usually a single identifier, but can also be
    /// composed of multiple tokens if not separated with a space.
    fn argument(&mut self) -> ParseResult<Expr<'a>> {
        let current = self.cursor.peek();
        let mut parts = Vec::new();
        let mut builder = String::new();
        let mut lexeme = current.value;

        //pushes current token then advance
        macro_rules! append_current {
            () => {
                let value = self.cursor.next()?.value;
                builder.push_str(value);
                if let Some(joined) = try_join_str(lexeme, value) {
                    lexeme = joined;
                } else {
                    lexeme = value;
                }
                ()
            };
        }

        match current.token_type {
            Dollar => parts.push(self.substitution()?),
            BackSlash => {
                //never retain first backslash
                self.cursor.next()?;
                //advance so we are not pointing to token after '\'
                //will append the escaped value (token after the backslash)
                append_current!();
            }
            _ => {
                append_current!();
            }
        };

        while !self.cursor.is_at_end() {
            let token = self.cursor.peek();
            let pivot = token.token_type;
            match pivot {
                Space => break,

                BackSlash => {
                    if self.cursor.advance(word_sep()).is_some() {
                        break;
                    }

                    //never retain first backslash
                    self.cursor.next()?;
                    //advance so we are not pointing to token after '\'
                    //will append the escaped value (token after the backslash)
                    append_current!();
                }

                Dollar => {
                    if !builder.is_empty() {
                        parts.push(Expr::Literal(Literal {
                            lexeme,
                            parsed: LiteralValue::String(builder.clone()),
                        }));
                        builder.clear();
                    }
                    parts.push(self.substitution()?);
                }
                _ if pivot.is_ponctuation() => break,
                _ => {
                    append_current!();
                }
            }
        }

        if !builder.is_empty() {
            parts.push(Expr::Literal(Literal {
                lexeme,
                parsed: LiteralValue::String(builder),
            }));
        }
        if parts.len() == 1 {
            return Ok(parts.pop().unwrap());
        }

        Ok(Expr::TemplateString(TemplateString {
            parts
        }))
    }
}

impl<'a> Parser<'a> {
    fn parse_number_value(&mut self) -> ParseResult<LiteralValue> {
        let token = self.cursor.next()?;
        match token.token_type {
            IntLiteral => Ok(LiteralValue::Int(token.value.parse::<i64>().map_err(
                |e| {
                    match e.kind() {
                        IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => self
                            .expected::<()>("Integer constant is too large.")
                            .unwrap_err(),
                        _ => self.mk_parse_error(e.to_string()),
                    }
                },
            )?)),
            FloatLiteral => Ok(LiteralValue::Float(
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

    use crate::parser::ParseError;

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn int_overflow() {
        let tokens = vec![Token::new(
            IntLiteral,
            "123456789012345678901234567890",
        )];
        let parsed = Parser::new(tokens).statement();
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
        let parsed = Parser::new(tokens).value().expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::Literal(Literal {
                lexeme: "'hello $world! $(this is a test) @(of course)'",
                parsed: "hello $world! $(this is a test) @(of course)".into(),
            })
        );
    }

    #[test]
    fn escaped_literal() {
        let tokens = lex("a\\a");
        let parsed = Parser::new(tokens).value().expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::Literal(Literal {
                lexeme: "a",
                parsed: "aa".into(),
            })
        );
    }

    #[test]
    fn missing_quote() {
        let tokens = vec![Token::new(Quote, "' command")];
        let parsed = Parser::new(tokens).statement();
        assert_eq!(
            parsed,
            Err(ParseError {
                message: "Unterminated string literal.".to_string(),
            })
        );
    }
}
