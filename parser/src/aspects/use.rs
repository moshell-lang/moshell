use lexer::token::TokenType;
use lexer::token::TokenType::{Comma, Identifier};
use crate::ast::Expr;
use crate::ast::r#use::Use;
use crate::moves::{eox, MoveOperations, of_type, repeat, spaces, word_sep};
use crate::parser::{Parser, ParseResult};

/// Parser aspect to parse use statements
pub trait UseAspect<'a> {
    ///parse a 'use x, y' statement
    fn parse_use(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> UseAspect<'a> for Parser<'a> {
    fn parse_use(&mut self) -> ParseResult<Expr<'a>> {
        self.cursor.force(of_type(TokenType::Use), "expected 'use'")?;

        //first identifier
        let mut uses = vec![
            self.cursor.force(
                spaces().then(of_type(Identifier)),
                "expected at least one identifier",
            )?.value
        ];

        //then parse others if any
        let mut tail: Vec<_> = self.cursor.select(
            repeat(
                repeat(word_sep()).then(of_type(Comma))
                    .then(repeat(word_sep()).then(of_type(Identifier)))
            )
        ).into_iter()
            .filter(|t| t.token_type == Identifier)
            .map(|t| t.value)
            .collect();

        //look for any trailing ','
        if self.cursor.lookahead(spaces().or(of_type(Comma))).is_some() {
            return self.expected("Unexpected comma ','");
        }
        self.cursor.force(spaces().then(eox()), "expected new line or semicolon.")?;

        uses.append(&mut tail);

        Ok(Expr::Use(Use {
            uses
        }))
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use lexer::lexer::lex;
    use crate::ast::Expr;
    use crate::ast::r#use::Use;
    use crate::parse;
    use crate::parser::ParseError;

    #[test]
    fn test_use() {
        let result = parse(lex("use TOKEN")).expect("parser failed");
        assert_eq!(
            result,
            vec![
                Expr::Use(Use {
                    uses: vec!["TOKEN"]
                })
            ]
        )
    }

    #[test]
    fn test_uses() {
        let result = parse(lex("use TOKEN,    A \\\n , B \\\n , C")).expect("parser failed");
        assert_eq!(
            result,
            vec![
                Expr::Use(Use {
                    uses: vec!["TOKEN", "A", "B", "C"]
                })
            ]
        )
    }

    #[test]
    fn test_use_trailing_comma() {
        let result = parse(lex("use TOKEN, A, B, "));
        assert_eq!(
            result,
            Err(ParseError {
                message: "Unexpected comma ','".to_string()
            })
        )
    }

    #[test]
    fn test_use_empty() {
        let result = parse(lex("use"));
        assert_eq!(
            result,
            Err(ParseError {
                message: "expected at least one identifier".to_string()
            })
        )
    }
}