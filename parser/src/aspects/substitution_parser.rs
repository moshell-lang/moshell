use crate::aspects::var_reference_parser::VarReferenceParser;
use crate::ast::substitution::{Substitution, SubstitutionKind};
use crate::ast::Expr;
use crate::moves::of_type;
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType;

pub(crate) trait SubstitutionParser<'a> {
    /// Parse a substitution expression, i.e. a variable reference or a command capture.
    fn substitution(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> SubstitutionParser<'a> for Parser<'a> {
    fn substitution(&mut self) -> ParseResult<Expr<'a>> {
        self.cursor
            .force(of_type(TokenType::Dollar), "Expected dollar sign.")?;
        if self
            .cursor
            .advance(of_type(TokenType::RoundedLeftBracket))
            .is_some()
        {
            let expr = Box::new(self.statement()?);
            self.cursor.force(
                of_type(TokenType::RoundedRightBracket),
                "Expected closing bracket.",
            )?;
            Ok(Expr::Substitution(Substitution {
                expr,
                kind: SubstitutionKind::Capture,
            }))
        } else {
            self.var_reference()
        }
    }
}
