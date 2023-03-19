use crate::err::ParseErrorKind;
use crate::moves::{eod, eox, of_type, word_seps, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::structure::Construct;
use ast::Expr;
use lexer::token::TokenType;

pub trait StructureAspect<'a> {
    /// Parses a structure constructor call.
    fn constructor(&mut self) -> ParseResult<Expr<'a>>;

    /// Checks if the cursor is at the start of a constructor.
    fn is_at_constructor_start(&self) -> bool;
}

impl<'a> StructureAspect<'a> for Parser<'a> {
    fn constructor(&mut self) -> ParseResult<Expr<'a>> {
        let name = self
            .cursor
            .force(of_type(TokenType::Identifier), "Expected structure name.")?;
        let open_parenthesis = self.cursor.force(
            of_type(TokenType::RoundedLeftBracket),
            "Expected opening parenthesis.",
        )?;
        self.delimiter_stack.push_back(open_parenthesis.clone());

        // Read the args until a closing delimiter or a new non-escaped line is found.
        let mut args = vec![];
        loop {
            if self
                .cursor
                .advance(word_seps().then(of_type(TokenType::RoundedRightBracket)))
                .is_some()
            {
                self.delimiter_stack.pop_back();
                return Ok(Expr::Construct(Construct {
                    name: name.value,
                    args,
                }));
            }
            args.push(self.next_value()?);
            self.cursor.advance(word_seps());

            // Check if the constructor is abnormally terminated.
            if self.cursor.lookahead(eox()).is_some() {
                self.expected(
                    "Expected closing parenthesis.",
                    ParseErrorKind::Unpaired(self.cursor.relative_pos(open_parenthesis.clone())),
                )?;
            }
            if self.cursor.lookahead(eod()).is_some() {
                self.expect_delimiter(TokenType::RoundedRightBracket)?;
                break;
            }
        }
        Ok(Expr::Construct(Construct {
            name: name.value,
            args,
        }))
    }

    fn is_at_constructor_start(&self) -> bool {
        self.cursor
            .lookahead(
                of_type(TokenType::Identifier).and_then(of_type(TokenType::RoundedLeftBracket)),
            )
            .is_some()
    }
}

#[cfg(test)]
mod tests {
    use context::source::Source;
    use pretty_assertions::assert_eq;

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use ast::structure::Construct;
    use ast::value::Literal;
    use ast::Expr;

    #[test]
    fn empty_constructor() {
        let source = Source::unknown("Foo()");
        let source2 = Source::unknown("Foo( )");
        let expr = Parser::new(source).parse().expect("Failed to parse");
        let expr2 = Parser::new(source2).parse().expect("Failed to parse");
        let expected = vec![Expr::Construct(Construct {
            name: "Foo",
            args: vec![],
        })];
        assert_eq!(expr, expected);
        assert_eq!(expr2, expected);
    }

    #[test]
    fn parse_constructor() {
        let source = Source::unknown("Foo(a 2 c)");
        let expr = Parser::new(source).parse().expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::Construct(Construct {
                name: "Foo",
                args: vec![
                    Expr::Literal("a".into()),
                    Expr::Literal(Literal {
                        lexeme: "2",
                        parsed: 2.into(),
                    }),
                    Expr::Literal("c".into()),
                ]
            })],
        );
    }

    #[test]
    fn constructor_with_newlines_and_space() {
        let source = Source::unknown("Foo( \\\nthis  \\\n  is\\\nfine)");
        let expr = Parser::new(source).parse().expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::Construct(Construct {
                name: "Foo",
                args: vec![
                    Expr::Literal("this".into()),
                    Expr::Literal("is".into()),
                    Expr::Literal("fine".into()),
                ]
            })],
        );
    }

    #[test]
    fn constructor_accept_string_literals() {
        let source = Source::unknown("Foo('===\ntesting something\n===' c)");
        let expr = Parser::new(source).parse().expect("Failed to parse");
        assert_eq!(
            expr,
            vec![Expr::Construct(Construct {
                name: "Foo",
                args: vec![
                    Expr::Literal(Literal {
                        lexeme: "'===\ntesting something\n==='",
                        parsed: "===\ntesting something\n===".into(),
                    }),
                    Expr::Literal("c".into())
                ]
            }),]
        );
    }

    #[test]
    fn constructor_with_unpaired_parenthesis() {
        let content = "Foo(a 2 c\n)";
        let source = Source::unknown(content);
        let expr: ParseResult<_> = parse(source).into();
        assert_eq!(
            expr,
            Err(ParseError {
                message: "Expected closing parenthesis.".into(),
                position: content.find('\n').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unpaired(content.find('(').map(|p| p..p + 1).unwrap())
            })
        )
    }

    #[test]
    fn constructor_exit_when_mismatched_bracket() {
        let content = "Foo(41 ]";
        let source = Source::unknown(content);
        let expr: ParseResult<_> = parse(source).into();
        assert_eq!(
            expr,
            Err(ParseError {
                message: "Mismatched closing delimiter.".into(),
                position: content.len() - 1..content.len(),
                kind: ParseErrorKind::Unpaired(content.find('(').map(|p| p..p + 1).unwrap())
            })
        )
    }
}
