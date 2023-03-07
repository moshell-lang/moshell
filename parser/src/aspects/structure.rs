use crate::ast::structure::Construct;
use crate::ast::Expr;
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
        self.cursor.force(
            of_type(TokenType::RoundedLeftBracket),
            "Expected opening parenthesis.",
        )?;
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
            if self.cursor.lookahead(eox().or(eod())).is_some() {
                self.cursor.force(
                    of_type(TokenType::RoundedRightBracket),
                    "Expected closing parenthesis.",
                )?;
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
    use crate::parser::Parser;

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
}
