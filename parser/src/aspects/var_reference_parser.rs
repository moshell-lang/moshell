use lexer::token::TokenType::*;

use crate::ast::variable::VarReference;
use crate::ast::Expr;
use crate::moves::{of_type, of_types};
use crate::parser::{ParseResult, Parser};

pub trait VarReferenceParser<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> VarReferenceParser<'a> for Parser<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>> {
        let has_bracket = self
            .cursor
            .advance(of_type(CurlyLeftBracket))
            .is_some();
        let name = self
            .cursor
            .force(of_types(&[
                Identifier, IntLiteral,
                Dollar, At, Not,
            ]), "Expected variable name.")?;
        if has_bracket {
            self.cursor.force(
                of_type(CurlyRightBracket),
                "Expected closing curly bracket.",
            )?;
        }
        Ok(Expr::VarReference(VarReference { name: name.clone() }))
    }
}

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
    use lexer::token::{Token};

    use crate::ast::variable::VarReference;
    use crate::ast::Expr;
    use pretty_assertions::assert_eq;
    use lexer::token::TokenType::Identifier;
    use crate::parse;

    #[test]
    fn test_simple_ref() {
        let tokens = lex("$VARIABLE");
        let ast = parse(tokens).expect("failed to parse");
        assert_eq!(
            ast,
            vec![
                Expr::VarReference(VarReference {
                    name: Token::new(Identifier, "VARIABLE")
                })
            ]
        )
    }

    #[test]
    fn test_wrapped_ref() {
        let tokens = lex("${VAR}IABLE");
        let ast = parse(tokens).expect("failed to parse");
        assert_eq!(
            ast,
            vec![
                Expr::VarReference(VarReference {
                    name: Token::new(Identifier, "VAR")
                }),
                Expr::Literal("IABLE".into())
            ]
        )
    }
}
