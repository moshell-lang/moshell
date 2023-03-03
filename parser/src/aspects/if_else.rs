use lexer::token::TokenType;
use lexer::token::TokenType::{Else, SemiColon};
use crate::ast::Expr;
use crate::moves::{MoveOperations, of_type, spaces, times};
use crate::parser::{Parser, ParseResult};
use crate::ast::flow_control::If;
pub trait IfElseAspect<'a> {
    fn parse_if(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> IfElseAspect<'a> for Parser<'a> {
    fn parse_if(&mut self) -> ParseResult<Expr<'a>> {
        self.cursor.force(
            of_type(TokenType::If),
            "expected 'if' at start of if expression"
        )?;
        let condition = self.expression_statement()?;


        if self.cursor.advance(times(2, spaces().then(of_type(SemiColon)))).is_some() {
            //if the if condition is followed by at least two semicolon, then the if body is invalid
            self.expected("Forbidden ';' expression after if condition")?
        }

        //the success_branch of the if
        let success_branch = self.statement()?;

        let fail_branch =
            if self.cursor.advance(spaces().then(of_type(Else))).is_some() {
                Some(self.statement()?)
            } else {
                None
            };

        Ok(Expr::If(If {
            condition: Box::new(condition),
            success_branch: Box::new(success_branch),
            fail_branch: Box::new(fail_branch)
        }))
    }
}