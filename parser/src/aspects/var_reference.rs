use lexer::token::TokenType;

use crate::ast::variable::VarReference;
use crate::ast::Expr;
use crate::moves::of_type;
use crate::parser::{ParseResult, Parser};

pub trait VarReferenceAspect<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> VarReferenceAspect<'a> for Parser<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>> {
        let has_bracket = self
            .cursor
            .advance(of_type(TokenType::CurlyLeftBracket))
            .is_some();
        let name = self
            .cursor
            .force(of_type(TokenType::Identifier), "Expected variable name.")?
            .value;
        if has_bracket {
            self.cursor.force(
                of_type(TokenType::CurlyRightBracket),
                "Expected closing curly bracket.",
            )?;
        }
        Ok(Expr::VarReference(VarReference { name }))
    }
}

#[cfg(test)]
mod tests {
    use crate::aspects::substitution::SubstitutionAspect;
    use crate::ast::variable::VarReference;
    use crate::ast::Expr;
    use crate::parser::Parser;
    use crate::source::Source;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_simple_ref() {
        let source = Source::unknown("$VARIABLE");
        let ast = Parser::new(source).substitution().expect("failed to parse");
        assert_eq!(ast, Expr::VarReference(VarReference { name: "VARIABLE" }))
    }

    #[test]
    fn test_wrapped_ref() {
        let source = Source::unknown("${VAR}IABLE");
        let ast = Parser::new(source).substitution().expect("failed to parse");
        assert_eq!(ast, Expr::VarReference(VarReference { name: "VAR" }))
    }
}
