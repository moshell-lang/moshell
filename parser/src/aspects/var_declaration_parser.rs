use lexer::token::TokenType;

use crate::ast::variable::{TypedVariable, VarDeclaration, VarKind};
use crate::ast::Expr;
use crate::moves::{MoveOperations, of_type, of_types, space};
use crate::parser::{ParseResult, Parser};

pub trait VarDeclarationParser<'a> {
    /// Parses a variable declaration.
    fn var_declaration(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> VarDeclarationParser<'a> for Parser<'a> {
    /// Parses a variable declaration.
    fn var_declaration(&mut self) -> ParseResult<Expr<'a>> {
        let cursor = self.cursor();
        //ensure that the expresssion starts with var or val
        cursor.force(of_types(&[TokenType::Var, TokenType::Val]), "expected var or val keywords");
        let name = cursor.force(space().and_then(of_type(TokenType::Identifier)), "Expected variable name.")?;

        let ty = match cursor.advance(of_type(TokenType::Colon)) {
            None => None,
            Some(_) => Some(cursor.force(of_type(TokenType::Identifier), "Expected variable type")?),
        }.map(|t| t.clone());

        let initializer = match cursor.advance(of_type(TokenType::Equal)) {
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

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
    use crate::aspects::var_declaration_parser::VarDeclarationParser;
    use crate::ast::Expr;
    use crate::parser::Parser;

    #[test]
    fn val__declaration() {
        let tokens = lex("$VARIABLE");
        let ast = Parser::new(tokens).var_declaration().expect("failed to parse");
        assert_eq!(ast, Expr::VarReference(VarReference {
            name: Token::new(TokenType::Identifier, "VARIABLE")
        }))
    }
}