use crate::aspects::var_reference_parser::VarReferenceParser;
use crate::ast::substitution::{Substitution, SubstitutionKind};
use crate::ast::Expr;
use crate::moves::{of_type, of_types, space, MoveOperations};
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType;

pub(crate) trait SubstitutionParser<'a> {
    /// Parse a substitution expression, i.e. a variable reference or a command capture.
    fn substitution(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> SubstitutionParser<'a> for Parser<'a> {
    fn substitution(&mut self) -> ParseResult<Expr<'a>> {
        let start_token = self.cursor.force(
            of_types(&[TokenType::At, TokenType::Dollar]),
            "Expected dollar sign.",
        )?;

        if self
            .cursor
            .advance(of_type(TokenType::RoundedLeftBracket))
            .is_none()
            && start_token.token_type == TokenType::Dollar
        {
            return self.var_reference();
        }

        let expr = Box::new(self.statement()?);
        self.cursor.force(
            space().then(of_type(TokenType::RoundedRightBracket)),
            "Expected closing bracket.",
        )?;

        Ok(Expr::Substitution(Substitution {
            expr,
            kind: if start_token.token_type == TokenType::At {
                SubstitutionKind::Return
            } else {
                SubstitutionKind::Capture
            },
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::aspects::substitution_parser::SubstitutionParser;
    use crate::parse;
    use crate::parser::{ParseError, Parser};
    use lexer::lexer::lex;
    use pretty_assertions::assert_eq;

    #[test]
    fn unterminated_substitution() {
        let tokens = lex("$(echo");
        let ast = Parser::new(tokens).substitution();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Expected closing bracket.".to_string()
            })
        );
    }

    #[test]
    fn unexpected_closing_parenthesis() {
        let tokens = lex("some stuff)");
        let ast = parse(tokens);
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Unexpected closing bracket.".to_string()
            })
        );
    }
}
