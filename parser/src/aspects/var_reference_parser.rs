use crate::ast::variable::VarReference;
use crate::ast::Expr;
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType;

pub trait VarReferenceParser<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> VarReferenceParser<'a> for Parser<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>> {
        let cursor = self.cursor();
        cursor.expect_token(TokenType::Dollar, "Expected dollar sign.")?;
        let has_bracket = cursor
            .match_token_space_aware(TokenType::CurlyLeftBracket)
            .is_some();
        let name =
            self.expect_token_space_aware(TokenType::Identifier, "Expected variable name.")?;
        if has_bracket {
            self.expect_token(
                TokenType::CurlyRightBracket,
                "Expected closing curly bracket.",
            )?;
        }
        Ok(Expr::VarReference(VarReference { name }))
    }
}
