use lexer::token::TokenType;

use crate::ast::variable::{TypedVariable, VarDeclaration, VarKind};
use crate::ast::Expr;
use crate::moves::{MoveOperations, of_type, space};
use crate::parser::{ParseResult, Parser};

pub trait VarDeclarationParser<'a> {
    /// Parses a variable declaration.
    fn var_declaration(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> VarDeclarationParser<'a> for Parser<'a> {
    /// Parses a variable declaration.
    fn var_declaration(&mut self) -> ParseResult<Expr<'a>> {
        let kind = match self.cursor.next()?.token_type {
            TokenType::Var => VarKind::Var,
            TokenType::Val => VarKind::Val,
            _ => return self.expected("expected var or val keywords"),
        };
        let name = self.cursor.force(space().and_then(of_type(TokenType::Identifier)), "Expected variable name.")?;

        let ty = match self.cursor.advance(of_type(TokenType::Colon)) {
            None => None,
            Some(_) => Some(self.cursor.force(of_type(TokenType::Identifier), "Expected variable type")?),
        }.map(|t| t.clone());

        let initializer = match self.cursor.advance(of_type(TokenType::Equal)) {
            None => None,
            Some(_) => Some(self.expression()?),
        };

        Ok(Expr::VarDeclaration(VarDeclaration {
            kind,
            var: TypedVariable {
                name: name.clone(),
                ty: ty.map(|t| t.clone()),
            },
            initializer: initializer.map(Box::new),
        }))
    }
}

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
    use lexer::token::Token;
    use crate::ast::Expr;
    use crate::parser::Parser;
    use super::*;

    #[test]
    fn val_declaration() {
        let tokens = lex("val variable");
        let ast = Parser::new(tokens).var_declaration().expect("failed to parse");
        assert_eq!(ast, Expr::VarDeclaration(VarDeclaration {
            kind: VarKind::Val,
            var: TypedVariable { name: Token::new(TokenType::Identifier, "var"), ty: None },
            initializer: None,
        }))
    }
}