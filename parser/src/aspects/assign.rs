use crate::ast::variable::Assign;
use crate::moves::{of_type, word_seps};
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType;

pub trait AssignAspect<'a> {
    /// Parses a variable assignment.
    fn parse_assign(&mut self) -> ParseResult<Assign<'a>>;
}

impl<'a> AssignAspect<'a> for Parser<'a> {
    fn parse_assign(&mut self) -> ParseResult<Assign<'a>> {
        let name = self
            .cursor
            .force(of_type(TokenType::Identifier), "Expected variable name.")?
            .value;
        self.cursor.advance(word_seps());
        self.cursor.force(
            of_type(TokenType::Equal),
            "Expected '=' at start of assignment",
        )?;
        let value = Box::new(self.value()?);
        Ok(Assign { name, value })
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::value::Literal;
    use crate::ast::variable::Assign;
    use crate::ast::Expr;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::ParseResult;
    use context::source::Source;

    #[test]
    fn assign_no_value() {
        let content = "a=";
        let source = Source::unknown(content);
        let res: ParseResult<_> = parse(source).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "Expected value".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unexpected,
            })
        );
    }

    #[test]
    fn assign() {
        let source = Source::unknown("a = 1");
        let ast = parse(source).expect("Failed to parse");
        assert_eq!(
            ast,
            vec![Expr::Assign(Assign {
                name: "a",
                value: Box::new(Expr::Literal(Literal {
                    lexeme: "1",
                    parsed: 1.into()
                }))
            })]
        );
    }
}
