use context::source::try_join_str;
use std::num::IntErrorKind;

use crate::aspects::substitution::SubstitutionAspect;
use lexer::token::TokenType::*;

use crate::err::ParseErrorKind;
use crate::moves::{eod, eox, like, next, of_type, word_seps, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::range::{FilePattern, Iterable};
use ast::value::{Literal, LiteralValue, TemplateString};
use ast::*;
use lexer::token::TokenType;

/// Describes if a literal should be parsed strictly or leniently.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralLeniency {
    /// Stops parsing in more cases.
    ///
    /// For instance, a raw argument will stop parsing if it encounters a
    /// programming token, such as `,` or `..`.
    Strict,

    /// Parses more things.
    ///
    /// It should only be used when parsing a literal in a context where
    /// classic shell syntax is allowed.
    Lenient,
}

/// A trait that contains all the methods for parsing literals.
pub(crate) trait LiteralAspect<'a> {
    /// Parses any literal, number, argument, string, string template
    fn literal(&mut self, leniency: LiteralLeniency) -> ParseResult<Expr<'a>>;

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
    fn argument(&mut self, leniency: LiteralLeniency) -> ParseResult<Expr<'a>>;
}

impl<'a> LiteralAspect<'a> for Parser<'a> {
    fn literal(&mut self, leniency: LiteralLeniency) -> ParseResult<Expr<'a>> {
        let token = self.cursor.peek();
        let pivot = token.token_type;
        let is_alone = leniency == LiteralLeniency::Strict
            || self
                .cursor
                .lookahead(
                    next().and_then(
                        eox()
                            .or(eod())
                            .or(word_seps())
                            .or(like(TokenType::is_call_bound)),
                    ),
                )
                .is_some();
        match pivot {
            IntLiteral | FloatLiteral if is_alone => self.number_literal().map(Expr::Literal),
            Quote => self.string_literal().map(Expr::Literal),
            DoubleQuote => self.templated_string_literal().map(Expr::TemplateString),

            _ if pivot.is_keyword() => self.expected(
                &format!("Unexpected keyword '{}'", token.value),
                ParseErrorKind::Unexpected,
            ),
            _ if pivot.is_ponctuation()
                || (leniency == LiteralLeniency::Strict && pivot.is_extended_ponctuation()) =>
            {
                self.expected(
                    &format!("Unexpected token '{}'.", token.value),
                    ParseErrorKind::Unexpected,
                )
            }

            _ => self.argument(leniency),
        }
    }

    fn number_literal(&mut self) -> ParseResult<Literal<'a>> {
        Ok(Literal {
            lexeme: self.cursor.peek().value,
            parsed: self.parse_number_value()?,
        })
    }

    fn string_literal(&mut self) -> ParseResult<Literal<'a>> {
        let start = self.cursor.force(of_type(Quote), "Expected quote.")?;
        let mut lexeme = start.value;

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
                    if token.token_type == Quote {
                        if let Some(joined) = try_join_str(lexeme, token.value) {
                            lexeme = joined;
                        }
                        break;
                    }
                    if token.token_type != BackSlash {
                        value.push_str(token.value);
                    }
                    if let Some(joined) = try_join_str(lexeme, token.value) {
                        lexeme = joined;
                    }
                    if token.token_type == BackSlash {
                        if let Some(next) = self.cursor.advance(next()) {
                            value.push_str(next.value);
                            if let Some(joined) = try_join_str(lexeme, next.value) {
                                lexeme = joined;
                            }
                        }
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
        let start = self.cursor.force(of_type(DoubleQuote), "Expected quote.")?;
        let mut lexeme = self.cursor.peek().value;
        let mut literal_value = String::new();
        let mut parts = Vec::new();
        loop {
            if self.cursor.is_at_end() {
                return self.expected(
                    "Unterminated string literal.",
                    ParseErrorKind::Unpaired(self.cursor.relative_pos(&start)),
                );
            }

            let current = self.cursor.peek();
            match current.token_type {
                DoubleQuote => {
                    self.cursor.advance(next());
                    break;
                }

                BackSlash => {
                    self.cursor.advance(next());
                    if let Some(joined) = try_join_str(lexeme, current.value) {
                        lexeme = joined;
                    }
                    if let Some(next) = self.cursor.advance(next()) {
                        literal_value.push_str(next.value);
                        if let Some(joined) = try_join_str(lexeme, next.value) {
                            lexeme = joined;
                        }
                    }
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

        Ok(TemplateString { parts })
    }

    /// Parses a single argument.
    ///
    /// An argument is usually a single identifier, but can also be
    /// composed of multiple tokens if not separated with a space.
    fn argument(&mut self, leniency: LiteralLeniency) -> ParseResult<Expr<'a>> {
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
                Space | NewLine => break,

                BackSlash => {
                    if self.cursor.advance(word_seps()).is_some() {
                        break;
                    }

                    if let Some(joined) = try_join_str(lexeme, token.value) {
                        lexeme = joined;
                    }
                    //never retain first backslash
                    self.cursor.next()?;
                    //advance so we are not pointing to token after '\'
                    //will append the escaped value (token after the backslash)
                    append_current!();
                }

                Dollar => {
                    if !builder.is_empty() {
                        parts.push(Self::literal_or_wildcard(builder.clone(), lexeme));
                        builder.clear();
                    }
                    parts.push(self.substitution()?);
                }
                _ if leniency == LiteralLeniency::Strict && pivot.is_extended_ponctuation() => {
                    break
                }
                _ if pivot.is_ponctuation() | pivot.is_identifier_bound() => break,
                _ => {
                    append_current!();
                }
            }
        }

        if !builder.is_empty() {
            parts.push(Self::literal_or_wildcard(builder.clone(), lexeme));
        }
        if parts.len() == 1 {
            return Ok(parts.pop().unwrap());
        }

        Ok(Expr::TemplateString(TemplateString { parts }))
    }
}

impl<'a> Parser<'a> {
    fn parse_number_value(&mut self) -> ParseResult<LiteralValue> {
        let token = self.cursor.next()?;
        match token.token_type {
            IntLiteral => Ok(LiteralValue::Int(token.value.parse::<i64>().map_err(
                |e| match e.kind() {
                    IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => self.mk_parse_error(
                        "Integer constant is too large.".to_string(),
                        token,
                        ParseErrorKind::InvalidFormat,
                    ),
                    _ => self.mk_parse_error(e.to_string(), token, ParseErrorKind::InvalidFormat),
                },
            )?)),
            FloatLiteral => Ok(LiteralValue::Float(token.value.parse::<f64>().map_err(
                |e| self.mk_parse_error(e.to_string(), token, ParseErrorKind::InvalidFormat),
            )?)),
            _ => self.expected("Expected a literal.", ParseErrorKind::Unexpected),
        }
    }

    fn literal_or_wildcard(read: String, lexeme: &'a str) -> Expr<'a> {
        if read.contains('*') {
            Expr::Range(Iterable::Files(FilePattern {
                lexeme,
                pattern: read,
            }))
        } else {
            Expr::Literal(Literal {
                lexeme,
                parsed: LiteralValue::String(read),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse;

    use super::*;
    use crate::err::ParseErrorKind::InvalidFormat;
    use crate::err::{ParseError, ParseErrorKind};
    use context::source::Source;
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
                kind: InvalidFormat,
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
                lexeme: "'hello $world! $(this is a test) @(of course)'",
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
                lexeme: "a\\a",
                parsed: "aa".into(),
            })
        );
    }

    #[test]
    fn escaped_string_literal() {
        let source = Source::unknown("'a\\'a'");
        let parsed = Parser::new(source).expression().expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::Literal(Literal {
                lexeme: "'a\\'a'",
                parsed: "a'a".into(),
            })
        );
    }

    #[test]
    fn escaped_template_string_literal() {
        let source = Source::unknown("\"a\\\"a'\"");
        let parsed = Parser::new(source).expression().expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::TemplateString(TemplateString {
                parts: vec![Expr::Literal(Literal {
                    lexeme: "a\\\"a'",
                    parsed: "a\"a'".into(),
                }),]
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
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unpaired(0..1),
            })
        );
    }
}
