use lexer::token::TokenType;

use crate::ast::variable::VarReference;
use crate::ast::Expr;
use crate::moves::of_type;
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
            .advance(of_type(TokenType::CurlyLeftBracket))
            .is_some();
        let name = self
            .cursor
            .force(of_type(TokenType::Identifier), "Expected variable name.")?;
        if has_bracket {
            self.cursor.force(
                of_type(TokenType::CurlyRightBracket),
                "Expected closing curly bracket.",
            )?;
        }
        Ok(Expr::VarReference(VarReference { name: name.clone() }))
    }
}

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
    use lexer::token::{Token, TokenType};

    use crate::aspects::substitution_parser::SubstitutionParser;
    use crate::ast::variable::VarReference;
    use crate::ast::Expr;
    use crate::parser::Parser;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_simple_ref() {
        let tokens = lex("$VARIABLE");
        let ast = Parser::new(tokens).substitution().expect("failed to parse");
        assert_eq!(
            ast,
            Expr::VarReference(VarReference {
                name: Token::new(TokenType::Identifier, "VARIABLE")
            })
        )
    }

    #[test]
    fn test_wrapped_ref() {
        let tokens = lex("${VAR}IABLE");
        let ast = Parser::new(tokens).substitution().expect("failed to parse");
        assert_eq!(
            ast,
            Expr::VarReference(VarReference {
                name: Token::new(TokenType::Identifier, "VAR")
            })
        )
    }
}
