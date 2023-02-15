use crate::aspects::base_parser::BaseParser;
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
        self.expect_token(TokenType::Dollar, "Expected dollar sign.")?;
        let name =
            self.expect_token_space_aware(TokenType::Identifier, "Expected variable name.")?;
        Ok(Expr::VarReference(VarReference { name }))
    }
}
