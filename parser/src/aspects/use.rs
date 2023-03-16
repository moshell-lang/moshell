use crate::err::ParseErrorKind;
use crate::moves::{eox, of_type, repeat, spaces, word_seps, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::r#use::Use;
use ast::Expr;
use lexer::token::TokenType;
use lexer::token::TokenType::{Comma, Identifier};

/// Parser aspect to parse use statements
pub trait UseAspect<'a> {
    ///parse a 'use x, y' statement
    fn parse_use(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> UseAspect<'a> for Parser<'a> {
    fn parse_use(&mut self) -> ParseResult<Expr<'a>> {
        self.cursor
            .force(of_type(TokenType::Use), "expected 'use'")?;

        //first identifier
        let mut uses = vec![
            self.cursor
                .force(
                    spaces().then(of_type(Identifier)),
                    "expected at least one identifier",
                )?
                .value,
        ];

        //then parse others if any
        let mut tail: Vec<_> = self
            .cursor
            .collect(repeat(
                word_seps()
                    .then(of_type(Comma))
                    .then(word_seps().then(of_type(Identifier))),
            ))
            .into_iter()
            .filter(|t| t.token_type == Identifier)
            .map(|t| t.value)
            .collect();

        //look for any trailing ','
        if self.cursor.lookahead(spaces().or(of_type(Comma))).is_some() {
            return self.expected("Unexpected comma ','", ParseErrorKind::Unexpected);
        }
        self.cursor
            .force(spaces().then(eox()), "expected new line or semicolon.")?;

        uses.append(&mut tail);

        Ok(Expr::Use(Use { uses }))
    }
}

#[cfg(test)]
mod tests {
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::ParseResult;
    use ast::r#use::Use;
    use ast::Expr;
    use context::source::Source;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_use() {
        let source = Source::unknown("use TOKEN");
        let result = parse(source).expect("parser failed");
        assert_eq!(
            result,
            vec![Expr::Use(Use {
                uses: vec!["TOKEN"]
            })]
        )
    }

    #[test]
    fn uses() {
        let source = Source::unknown("use TOKEN,    A \\\n , B \\\n , C");
        let result = parse(source).expect("parser failed");
        assert_eq!(
            result,
            vec![Expr::Use(Use {
                uses: vec!["TOKEN", "A", "B", "C"]
            })]
        )
    }

    #[test]
    fn use_trailing_comma() {
        let content = "use TOKEN, A, B, ";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Unexpected comma ','".to_string(),
                position: content.rfind(',').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn use_empty() {
        let content = "use";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "expected at least one identifier".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }
}
