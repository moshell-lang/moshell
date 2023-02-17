use crate::ast::variable::VarReference;
use crate::ast::Expr;
use crate::parser::{ParseResult, Parser};
use lexer::token::{Token, TokenType};
use crate::moves::{MoveOperations, of_type, space};

pub trait VarReferenceParser<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> VarReferenceParser<'a> for Parser<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>> {
        let cursor = self.cursor();
        cursor.force(of_type(TokenType::Dollar), "Expected dollar sign.")?;
        let has_bracket = cursor
            .advance(space().and_then(of_type(TokenType::CurlyLeftBracket)))
            .is_some();
        let name =
            cursor.force(space().and_then(of_type(TokenType::Identifier)), "Expected variable name.")?;
        if has_bracket {
            cursor.force(
                of_type(TokenType::CurlyRightBracket),
                "Expected closing curly bracket.",
            )?;
        }
        Ok(Expr::VarReference(VarReference { name: name.clone() }))
    }
}
