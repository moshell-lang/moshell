use lexer::token::TokenType;
use crate::ast::{Expr, TypedVariable, VarDeclaration, VarKind};
use crate::aspects::base_parser::BaseParser;
use crate::parser::{Parser, ParseResult};

pub trait VarDeclarationParser<'a> {
    /// Parses a variable declaration.
    fn var_declaration(&mut self, kind: VarKind) -> ParseResult<Expr<'a>>;
}

impl<'a> VarDeclarationParser<'a> for Parser<'a> {
    /// Parses a variable declaration.
    fn var_declaration(&mut self, kind: VarKind) -> ParseResult<Expr<'a>> {
        let name = self.expect_token(TokenType::Identifier, "Expected variable name.")?;

        let ty = match self.match_token(TokenType::Colon) {
            None => None,
            Some(_) => Some(self.expect_token(TokenType::Identifier, "Expected variable type")?),
        };

        let initializer = match self.match_token(TokenType::Equal) {
            None => None,
            Some(_) => Some(self.expression()?)
        };

        Ok(Expr::VarDeclaration(VarDeclaration {
            kind,
            var: TypedVariable {
                name: name.clone(),
                ty,
            },
            initializer: initializer.map(Box::new),
        }))
    }
}