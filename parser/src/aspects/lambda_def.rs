use ast::lambda::LambdaDef;
use lexer::token::TokenType::{FatArrow, RoundedLeftBracket, RoundedRightBracket};
use crate::aspects::expr_list::ExpressionListAspect;
use crate::aspects::var_declaration::VarDeclarationAspect;
use crate::err::ParseErrorKind::Expected;
use crate::moves::{blanks, MoveOperations, of_type};
use crate::parser::{Parser, ParseResult};

pub trait LambdaDefinitionAspect<'a> {
    fn parse_lambda_definition(&mut self) -> ParseResult<LambdaDef<'a>>;
}

impl<'a> LambdaDefinitionAspect<'a> for Parser<'a> {
    fn parse_lambda_definition(&mut self) -> ParseResult<LambdaDef<'a>> {
        let args = self.parse_implicit_list(
            RoundedLeftBracket, RoundedRightBracket,
            false, Self::parse_typed_var,
        )?;
        self.cursor.force_with(
            blanks().then(of_type(FatArrow)),
            "expected lambda arrow",
            Expected("=>".to_string())
        )?;
        let body = Box::new(self.value()?);
        Ok(LambdaDef {
            args,
            body,
        })
    }
}

