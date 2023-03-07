use crate::ast::structure::Construct;
use crate::ast::Expr;
use crate::err::ParseErrorKind;
use crate::moves::{eod, eox, of_type, word_seps, MoveOperations};
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType;

pub trait StructureAspect<'a> {
    fn constructor(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> StructureAspect<'a> for Parser<'a> {
    fn constructor(&mut self) -> ParseResult<Expr<'a>> {
        let name = self
            .cursor
            .force(of_type(TokenType::Identifier), "Excepted structure name.")?;
        let open_parenthesis = self.cursor.force(
            of_type(TokenType::RoundedLeftBracket),
            "Expected opening parenthesis.",
        )?;
        self.delimiter_stack.push_back(open_parenthesis.clone());
        let mut args = vec![];
        loop {
            if self
                .cursor
                .advance(word_seps().then(of_type(TokenType::RoundedRightBracket)))
                .is_some()
            {
                return Ok(Expr::Construct(Construct {
                    name: name.value,
                    args,
                }));
            }
            args.push(self.next_value()?);
            self.cursor.advance(word_seps());
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
}

#[cfg(test)]
mod tests {
    use context::source::Source;
    use pretty_assertions::assert_eq;

    use crate::ast::structure::Construct;
    use crate::ast::value::Literal;
    use crate::ast::Expr;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};

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
        assert_eq!(expr, expected,);
        assert_eq!(expr2, expected,);
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
