use lexer::token::TokenType;
use crate::ast::Expr;
use crate::ast::r#match::{Match, MatchArm};
use crate::moves::of_type;
use crate::parser::{Parser, ParseResult};

pub trait MatchAspect<'a> {
    fn parse_match<P>(&mut self, parse_arm: P) -> ParseResult<Match<'a>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>>;
}

impl<'a> MatchAspect<'a> for Parser<'a> {
    fn parse_match<P>(&mut self, parse_arm: P) -> ParseResult<Match<'a>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>> {
        self.cursor.force(
            of_type(TokenType::Match),
            "expected 'match' keyword at start of match expression.",
        )?;

        let operand = Box::new(self.expression_statement()?);

        let arms = self.parse_match_arms()?;

        Ok(Match {
            operand,
            arms
        })
    }
}

impl<'a> Parser<'a> {
    fn parse_match_arms(&mut self) -> ParseResult<Vec<MatchArm<'a>>> {
        todo!()
    }
}