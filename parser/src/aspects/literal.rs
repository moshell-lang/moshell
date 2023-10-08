use std::num::IntErrorKind;

use ast::range::{FilePattern, Iterable};
use ast::value::{Literal, LiteralValue, TemplateString};
use ast::*;
use context::source::{try_join_str, SourceSegmentHolder};
use lexer::token::Token;
use lexer::token::TokenType::*;
use lexer::unescape;

use crate::aspects::substitution::SubstitutionAspect;
use crate::err::ParseErrorKind;
use crate::moves::{next, of_type, of_types, MoveOperations};
use crate::parser::{ParseResult, Parser};

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
    fn number_literal(&mut self) -> ParseResult<Literal>;

    /// Parses a string literal expression.
    ///
    /// This method is only used for single quoted strings.
    fn string_literal(&mut self) -> ParseResult<Literal>;

    /// Parses a string template literal expression.
    ///
    /// This method is only used for double quoted strings, which may contain variable references for instance.
    fn templated_string_literal(&mut self) -> ParseResult<TemplateString<'a>>;

    /// Parses a shell string with backticks.
    fn back_string_literal(&mut self) -> ParseResult<TemplateString<'a>>;

    /// Parse a raw argument.
    ///
    /// Arguments are not quoted and are separated by spaces.
    fn argument(&mut self, leniency: LiteralLeniency) -> ParseResult<Expr<'a>>;
}

impl<'a> LiteralAspect<'a> for Parser<'a> {
    fn literal(&mut self, leniency: LiteralLeniency) -> ParseResult<Expr<'a>> {
        let token = self.cursor.peek();
        let pivot = token.token_type;
        match pivot {
            StringLiteral => self.string_literal().map(Expr::Literal),
            StringStart => self.templated_string_literal().map(Expr::TemplateString),
            False | True if leniency == LiteralLeniency::Strict => {
                self.boolean_literal().map(Expr::Literal)
            }
            _ if pivot.is_keyword() => self.expected(
                format!("Unexpected keyword '{}'", token.value),
                ParseErrorKind::Unexpected,
            ),
            _ if pivot.is_ponctuation()
                || (leniency == LiteralLeniency::Strict && pivot.is_extended_ponctuation()) =>
            {
                self.expected(
                    format!("Unexpected token '{}'.", token.value),
                    ParseErrorKind::Unexpected,
                )
            }

            _ if leniency == LiteralLeniency::Lenient => self.argument(leniency),
            IntLiteral | FloatLiteral => self.number_literal().map(Expr::Literal),
            Dollar => self.substitution(),
            Identifier
                if self
                    .cursor
                    .lookahead(next().then(of_types(&[StringLiteral, StringStart])))
                    .is_some() =>
            {
                if token.value == "p" {
                    self.cursor.next_opt();
                    let literal = self.string_literal()?;
                    let segment = self.cursor.relative_pos(token).start..literal.segment.end;
                    Ok(Expr::Range(Iterable::Files(FilePattern {
                        pattern: match literal.parsed {
                            LiteralValue::String(s) => s,
                            _ => unreachable!(),
                        },
                        segment,
                    })))
                } else {
                    self.expected("Unexpected string format", ParseErrorKind::Unexpected)
                }
            }
            _ => self.expected("Unexpected word literal", ParseErrorKind::Unexpected),
        }
    }

    fn number_literal(&mut self) -> ParseResult<Literal> {
        let start = self.cursor.next()?;
        let value = start.value;
        Ok(Literal {
            parsed: self.parse_number_value(start)?,
            segment: self.cursor.relative_pos(value),
        })
    }

    fn string_literal(&mut self) -> ParseResult<Literal> {
        let literal = self
            .cursor
            .force(of_type(StringLiteral), "Expected string literal.")?;
        let segment = self.cursor.relative_pos(literal.value);
        Ok(Literal {
            parsed: LiteralValue::String(unescape(literal.value)),
            segment: (segment.start - 1)..(segment.end + 1),
        })
    }

    fn templated_string_literal(&mut self) -> ParseResult<TemplateString<'a>> {
        let start = self.cursor.force(of_type(StringStart), "Expected quote.")?;
        let mut parts = Vec::new();
        let end = loop {
            let token = self.cursor.peek();
            match token.token_type {
                StringEnd => {
                    self.cursor.next_opt();
                    break token;
                }
                StringContent => {
                    self.cursor.next_opt();
                    parts.push(Expr::Literal(Literal {
                        parsed: LiteralValue::String(unescape(token.value)),
                        segment: self.cursor.relative_pos(token.value),
                    }));
                }
                Dollar => {
                    parts.push(self.substitution()?);
                }
                EndOfFile => {
                    return self.expected(
                        "Unterminated string template literal.",
                        ParseErrorKind::Unpaired(self.cursor.relative_pos_ctx(start)),
                    )
                }
                _ => {
                    return self.expected(
                        "Unexpected token in string template literal.",
                        ParseErrorKind::Unexpected,
                    )
                }
            }
        };
        Ok(TemplateString {
            parts,
            segment: self.cursor.relative_pos_ctx(start..end),
        })
    }

    fn back_string_literal(&mut self) -> ParseResult<TemplateString<'a>> {
        let start = self.cursor.force(of_type(Backtick), "Expected backtick.")?;
        let mut parts = Vec::new();
        let end = loop {
            let token = self.cursor.peek();
            match token.token_type {
                Backtick => {
                    self.cursor.next_opt();
                    break token;
                }
                Dollar => {
                    parts.push(self.substitution()?);
                }
                EndOfFile => {
                    return self.expected(
                        "Unterminated string template literal.",
                        ParseErrorKind::Unpaired(self.cursor.relative_pos_ctx(start)),
                    )
                }
                _ => {
                    self.cursor.next_opt();
                    parts.push(Expr::Literal(Literal {
                        parsed: LiteralValue::String(unescape(token.value)),
                        segment: self.cursor.relative_pos(token.value),
                    }));
                }
            }
        };
        Ok(TemplateString {
            parts,
            segment: self.cursor.relative_pos_ctx(start..end),
        })
    }

    /// Parses a single argument.
    ///
    /// An argument is usually a single identifier, but can also be
    /// composed of multiple tokens if not separated with a space.
    fn argument(&mut self, leniency: LiteralLeniency) -> ParseResult<Expr<'a>> {
        let current = self.cursor.peek();
        let mut end = self.cursor.relative_pos(current.value).end;
        let mut parts = Vec::new();
        let mut builder = String::new();
        let mut lexeme = current.value;
        let mut non_string: Option<Token> = None;

        //pushes current token then advance
        macro_rules! append_current {
            () => {
                let token = self.cursor.next()?;
                let value = token.value;
                if matches!(token.token_type, IntLiteral | FloatLiteral | True | False) {
                    non_string = Some(token);
                }
                builder.push_str(value);
                if lexeme.is_empty() {
                    lexeme = value;
                } else if let Some(joined) = try_join_str(self.source.source, lexeme, value) {
                    lexeme = joined;
                } else {
                    lexeme = value;
                }
                end = self.cursor.relative_pos(value).end;
                ()
            };
        }

        match current.token_type {
            Dollar => {
                parts.push(self.substitution()?);
                lexeme = "";
            }
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
                    //never retain first backslash
                    self.cursor.next()?;
                }

                Dollar => {
                    if !builder.is_empty() {
                        parts.push(self.literal_or_wildcard(
                            builder.clone(),
                            lexeme,
                            non_string.take(),
                        )?);
                        builder.clear();
                    }
                    let substitution = self.substitution()?;
                    end = substitution.segment().end;
                    parts.push(substitution);
                }
                StringLiteral => {
                    if !builder.is_empty() {
                        parts.push(self.literal_or_wildcard(
                            builder.clone(),
                            lexeme,
                            non_string.take(),
                        )?);
                        builder.clear();
                    }
                    let literal = self.string_literal()?;
                    end = literal.segment.end;
                    parts.push(Expr::Literal(literal));
                }
                StringStart => {
                    if !builder.is_empty() {
                        parts.push(self.literal_or_wildcard(
                            builder.clone(),
                            lexeme,
                            non_string.take(),
                        )?);
                        builder.clear();
                    }
                    let template = self.templated_string_literal()?;
                    end = template.segment.end;
                    parts.extend(template.parts);
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
            parts.push(self.literal_or_wildcard(builder.clone(), lexeme, non_string)?);
        }
        if parts.len() == 1 {
            return Ok(parts.pop().unwrap());
        }

        let start = self.cursor.relative_pos_ctx(current).start;
        let segment = start..end;
        Ok(Expr::TemplateString(TemplateString { parts, segment }))
    }
}

pub(crate) fn literal_expr(source: &str, literal: &str) -> Literal {
    let idx = source.find(literal).unwrap();
    Literal {
        parsed: match literal.chars().next().unwrap() {
            '"' | '\'' => literal[1..literal.len() - 1].into(),
            _ => literal.into(),
        },
        segment: idx..idx + literal.len(),
    }
}

impl<'a> Parser<'a> {
    fn boolean_literal(&mut self) -> ParseResult<Literal> {
        let token = self.cursor.next()?;
        Ok(Literal {
            parsed: LiteralValue::Bool(match token.token_type {
                True => true,
                False => false,
                _ => {
                    return self.expected("Expected a boolean literal.", ParseErrorKind::Unexpected)
                }
            }),
            segment: self.cursor.relative_pos_ctx(token),
        })
    }

    fn parse_number_value(&self, token: Token<'a>) -> ParseResult<LiteralValue> {
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

    fn literal_or_wildcard(
        &self,
        read: String,
        lexeme: &'a str,
        numeric: Option<Token>,
    ) -> ParseResult<Expr<'a>> {
        let segment = self.cursor.relative_pos(lexeme);
        if let Some(token) = numeric {
            if token.value == read {
                return Ok(Expr::Literal(Literal {
                    parsed: match token.token_type {
                        True => LiteralValue::Bool(true),
                        False => LiteralValue::Bool(false),
                        _ => self.parse_number_value(token)?,
                    },
                    segment,
                }));
            }
        }
        Ok(if read.contains('*') {
            Expr::Range(Iterable::Files(FilePattern {
                pattern: read,
                segment,
            }))
        } else {
            Expr::Literal(Literal {
                parsed: LiteralValue::String(read),
                segment,
            })
        })
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::variable::VarReference;
    use context::source::{Source, SourceSegmentHolder};
    use context::str_find::find_in;

    use crate::err::ParseErrorKind::InvalidFormat;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::source::literal;

    use super::*;

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
    fn int_but_str() {
        let source = Source::unknown("5@5");
        let parsed = Parser::new(source)
            .parse_specific(Parser::call_argument)
            .expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::Literal(Literal {
                parsed: LiteralValue::String("5@5".into()),
                segment: source.segment(),
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
                parsed: "hello $world! $(this is a test) @(of course)".into(),
                segment: source.segment()
            })
        );
    }

    #[test]
    fn escaped_literal() {
        let source = Source::unknown("a\\a");
        let parsed = Parser::new(source)
            .call_argument()
            .expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::Literal(Literal {
                parsed: "aa".into(),
                segment: source.segment()
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
                parsed: "a'a".into(),
                segment: source.segment()
            })
        );
    }

    #[test]
    fn empty_template_string_literal() {
        let source = Source::unknown("\"\"");
        let parsed = Parser::new(source).expression().expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::TemplateString(TemplateString {
                parts: vec![],
                segment: source.segment()
            })
        );
    }

    #[test]
    fn escaped_template_string_literal() {
        let source = Source::unknown(r#""a\"a'""#);
        let parsed = Parser::new(source).expression().expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::TemplateString(TemplateString {
                parts: vec![Expr::Literal(Literal {
                    parsed: "a\"a'".into(),
                    segment: find_in(source.source, r#"a\"a'"#)
                })],
                segment: source.segment()
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

    #[test]
    fn missing_backtick() {
        let content = "`foo";
        let source = Source::unknown(content);
        let parsed: ParseResult<_> = parse(source).into();
        assert_eq!(
            parsed,
            Err(ParseError {
                message: "Unterminated string template literal.".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unpaired(0..1),
            })
        );
    }

    #[test]
    fn prefixed_arg() {
        let source = Source::unknown("+'hello'");
        let parsed = Parser::new(source)
            .parse_specific(Parser::call_argument)
            .expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::TemplateString(TemplateString {
                parts: vec![
                    literal(source.source, "+"),
                    literal(source.source, "'hello'"),
                ],
                segment: source.segment()
            })
        );
    }

    #[test]
    fn url_placeholder() {
        let source = Source::unknown("\"http://localhost:$NGINX_PORT\"");
        let parsed = Parser::new(source).expression().expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::TemplateString(TemplateString {
                parts: vec![
                    literal(source.source, "http://localhost:"),
                    Expr::VarReference(VarReference {
                        name: "NGINX_PORT",
                        segment: find_in(source.source, "$NGINX_PORT")
                    }),
                ],
                segment: source.segment()
            })
        );
    }
}
