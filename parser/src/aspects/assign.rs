use ast::variable::Assign;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType;

use crate::moves::{of_type, spaces};
use crate::parser::{ParseResult, Parser};

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
        self.cursor.advance(spaces());
        self.cursor.force(
            of_type(TokenType::Equal),
            "Expected '=' at start of assignment",
        )?;
        let value = Box::new(self.value()?);
        let segment = self.cursor.relative_pos(name).start..value.segment().end;
        Ok(Assign {
            name,
            value,
            segment,
        })
    }
}

#[cfg(test)]
mod tests {
    use ast::value::Literal;
    use ast::variable::Assign;
    use ast::Expr;
    use context::source::{Source, SourceSegmentHolder};

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::ParseResult;

    #[test]
    fn assign_no_value() {
        let content = "a=";
        let source = Source::unknown(content);
        let res: ParseResult<_> = parse(source).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "Expected expression".to_string(),
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
                    parsed: 1.into(),
                    segment: 4..5,
                })),
                segment: source.segment(),
            })]
        );
    }
}
