use lexer::token::TokenType;

use crate::aspects::base_parser::BaseParser;
use crate::ast::variable::{TypedVariable, VarDeclaration, VarKind};
use crate::ast::Expr;
use crate::moves::{MoveOperations, of_type, space};
use crate::parser::{ParseResult, Parser};

pub trait VarDeclarationParser<'a> {
    /// Parses a variable declaration.
    fn var_declaration(&mut self, kind: VarKind) -> ParseResult<Expr<'a>>;
}

impl<'a> VarDeclarationParser<'a> for Parser<'a> {
    /// Parses a variable declaration.
    fn var_declaration(&mut self, kind: VarKind) -> ParseResult<Expr<'a>> {
        let cursor = self.cursor();
        match kind {
            VarKind::Var => cursor.force(of_type(TokenType::Var), "Expected 'var' keyword")?,
            VarKind::Val => cursor.force(of_type(TokenType::Val), "Expected 'val' keyword.")?,
        };
        let name = cursor.force(space().and_then(of_type(TokenType::Identifier)), "Expected variable name.")?;

        let ty = match cursor.match_token(TokenType::Colon) {
            None => None,
            Some(_) => Some(cursor.expect_token(TokenType::Identifier, "Expected variable type")?),
        }.map(|t| t.clone());

        let initializer = match cursor.match_token(TokenType::Equal) {
            None => None,
            Some(_) => Some(self.expression()?),
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
