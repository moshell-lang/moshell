use std::num::IntErrorKind;

use ast::range::{FilePattern, Iterable};
use ast::value::{Literal, LiteralValue, TemplateString};
use ast::variable::{Tilde, TildeExpansion};
use ast::*;
use context::source::SourceSegmentHolder;
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
    fn argument(&mut self) -> ParseResult<Expr<'a>>;
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

            _ if leniency == LiteralLeniency::Lenient => self.argument(),
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
    fn argument(&mut self) -> ParseResult<Expr<'a>> {
        let start = self.cursor.peek();
        let mut parts = Vec::<Expr>::new();
        let mut wildcards = Vec::<usize>::new();
        let mut end_segment = self.cursor.relative_pos(start.value);

        if self.cursor.advance(of_type(Tilde)).is_some() {
            // Create the start of the tilde expansion, that may be completed later.
            if let Some(plus) = self.cursor.advance(of_type(Plus)) {
                parts.push(Expr::Tilde(TildeExpansion {
                    structure: Tilde::WorkingDir,
                    segment: end_segment.start..self.cursor.relative_pos_ctx(plus.value).end,
                }));
                end_segment = self.cursor.relative_pos(plus.value);
            } else {
                parts.push(Expr::Tilde(TildeExpansion {
                    structure: Tilde::HomeDir(None),
                    segment: end_segment.clone(),
                }));
            }
        }

        loop {
            let token = self.cursor.peek();
            match token.token_type {
                Space | NewLine | EndOfFile => break,
                Dollar => {
                    let substitution = self.substitution()?;
                    end_segment = substitution.segment();
                    parts.push(substitution);
                }
                StringStart => {
                    let template = self.templated_string_literal()?;
                    end_segment = template.segment();
                    parts.extend(template.parts);
                }
                StringLiteral => {
                    let literal = self.string_literal()?;
                    end_segment = literal.segment();
                    parts.push(Expr::Literal(literal));
                }
                _ => {
                    if token.token_type.is_ponctuation() {
                        break;
                    }
                    self.cursor.next_opt();
                    end_segment = self.cursor.relative_pos(token.value);

                    // Either append to the last literal, or create a new one.
                    // The third case is when a tilde expansion is present and can be completed.
                    match parts.last_mut() {
                        Some(Expr::Literal(Literal {
                            parsed: LiteralValue::String(ref mut s),
                            segment,
                        })) => {
                            s.push_str(token.value);
                            segment.end = end_segment.end;
                        }
                        Some(Expr::Tilde(TildeExpansion {
                            structure: Tilde::HomeDir(ref mut user @ None),
                            segment,
                        })) if token.token_type != Slash => {
                            *user = Some(Box::new(Expr::Literal(Literal {
                                parsed: LiteralValue::String(token.value.to_owned()),
                                segment: end_segment.clone(),
                            })));
                            segment.end = end_segment.end;
                        }
                        _ => {
                            parts.push(Expr::Literal(Literal {
                                parsed: LiteralValue::String(token.value.to_owned()),
                                segment: end_segment.clone(),
                            }));
                        }
                    }

                    // Preserve the position of wildcards.
                    if token.token_type == Star {
                        match wildcards.last().copied() {
                            Some(len) if len == parts.len() - 1 => {}
                            _ => wildcards.push(parts.len() - 1),
                        }
                    }
                }
            }
        }

        // Replace wildcards with the appropriate expression.
        for wildcard in wildcards {
            let expr = parts.get_mut(wildcard).unwrap();
            if let Expr::Literal(Literal {
                parsed: LiteralValue::String(pattern),
                segment,
            }) = expr
            {
                *expr = Expr::Range(Iterable::Files(FilePattern {
                    pattern: std::mem::take(pattern),
                    segment: segment.clone(),
                }));
            }
        }

        let start_segment = self.cursor.relative_pos(start.value);
        Ok(if parts.len() == 1 {
            // Reduce nesting if there is only one part.
            if start_segment == end_segment {
                // If the argument is a single token, it may be a numeric or a boolean literal.
                Expr::Literal(Literal {
                    parsed: match start.token_type {
                        True => LiteralValue::Bool(true),
                        False => LiteralValue::Bool(false),
                        IntLiteral | FloatLiteral => self.parse_number_value(start)?,
                        _ => {
                            return Ok(parts.pop().unwrap());
                        }
                    },
                    segment: start_segment,
                })
            } else {
                parts.pop().unwrap()
            }
        } else {
            Expr::TemplateString(TemplateString {
                parts,
                segment: start_segment.start..end_segment.end,
            })
        })
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
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::variable::{VarName, VarReference};
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
                        name: VarName::User("NGINX_PORT"),
                        segment: find_in(source.source, "$NGINX_PORT")
                    }),
                ],
                segment: source.segment()
            })
        );
    }

    #[test]
    fn user_home() {
        let source = Source::unknown("~test");
        let parsed = Parser::new(source)
            .literal(LiteralLeniency::Lenient)
            .expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::Tilde(TildeExpansion {
                structure: Tilde::HomeDir(Some(Box::new(literal(source.source, "test")))),
                segment: source.segment()
            })
        );
    }

    #[test]
    fn argument_wildcard() {
        let source = Source::unknown("foo*");
        let parsed = Parser::new(source)
            .literal(LiteralLeniency::Lenient)
            .expect("Failed to parse.");
        assert_eq!(
            parsed,
            Expr::Range(Iterable::Files(FilePattern {
                pattern: "foo*".into(),
                segment: source.segment()
            }))
        );
    }
}
