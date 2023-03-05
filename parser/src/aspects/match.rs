use lexer::token::TokenType;
use lexer::token::TokenType::{At, CurlyLeftBracket, CurlyRightBracket, FatArrow, Identifier, Bar, If};

use crate::aspects::literal::LiteralAspect;
use crate::ast::Expr;
use crate::ast::r#match::{Match, MatchArm, MatchPattern};
use crate::ast::r#match::MatchPattern::{Literal, Template, Wildcard};
use crate::moves::{any, blank, MoveOperations, of_type, of_types, repeat};
use crate::parser::{Parser, ParseResult};

pub trait MatchAspect<'a> {
    fn parse_match<P>(&mut self, parse_arm: P) -> ParseResult<Match<'a>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>> + Clone;
}

impl<'a> MatchAspect<'a> for Parser<'a> {
    fn parse_match<P>(&mut self, parse_arm: P) -> ParseResult<Match<'a>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>> + Clone {
        self.cursor.force(
            of_type(TokenType::Match),
            "expected 'match' keyword at start of match expression.",
        )?;

        let operand = Box::new(self.expression_statement()?);

        let arms = self.parse_match_arms(parse_arm)?;

        Ok(Match {
            operand,
            arms
        })
    }
}

impl<'a> Parser<'a> {
    fn parse_match_arms<P>(&mut self, parse_arm: P) -> ParseResult<Vec<MatchArm<'a>>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>> + Clone {
        self.cursor.force(repeat(blank().then(of_type(CurlyLeftBracket))), "expected '{'")?;

        let mut arms: Vec<MatchArm<'a>> = Vec::new();

        while self.cursor.advance(blank().then(of_type(CurlyRightBracket))).is_none() {
            let arm = self.parse_match_arm(parse_arm.clone())?;
            arms.push(arm);
        }

        Ok(arms)
    }

    fn parse_match_arm<P>(&mut self, parse_arm: P) -> ParseResult<MatchArm<'a>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>> {
        self.cursor.advance(repeat(blank())); //consume blanks

        let val_name = self.parse_extracted_name()?;
        let patterns = self.parse_patterns()?;
        let guard = self.parse_guard()?;
        let body = Box::new(self.parse_body(parse_arm)?);

        Ok(MatchArm {
            val_name,
            patterns,
            guard,
            body,
        })
    }

    fn parse_extracted_name(&mut self) -> ParseResult<Option<&'a str>> {
        if self.cursor.lookahead(
            any().then(of_type(At))
        ).is_some() {
            self.cursor.force(of_type(Identifier), "expected identifier for extracted val name")
                .map(|t| t.value)
                .map(Some)
        } else {
            Ok(None)
        }
    }

    fn parse_patterns(&mut self) -> ParseResult<Vec<MatchPattern<'a>>> {
        macro_rules! is_at_pattern_end {
            () =>{ self.cursor.lookahead(blank().then(of_types(&[If, FatArrow]))).is_some() }
        }

        if is_at_pattern_end!() {
            return self.expected("required pattern")
        }

        let first = self.parse_pattern()?;

        if first == Wildcard {
            return if !is_at_pattern_end!() {
                self.expected("wildcard pattern cannot be followed with other patterns")
            } else {
                Ok(vec![first])
            }
        }

        let mut patterns = vec![first];

        while self.cursor.advance(blank().then(of_type(Bar))).is_some() {
            let pattern = self.parse_pattern()?;
            if pattern == Wildcard {
                return self.expected("unexpected wildcard");
            }
            patterns.push(pattern)
        }

        if !is_at_pattern_end!() {
            let token = self.cursor.peek().value;
            return self.expected(&format!("unexpected token, expected 'if' or '=>', found '{token}'"))
        }

        Ok(patterns)
    }


    fn parse_pattern(&mut self) -> ParseResult<MatchPattern<'a>> {
        self.cursor.advance(repeat(blank())); //consume blanks;

        match self.cursor.peek().token_type {
            TokenType::Star => Ok(Wildcard),
            _ => match self.literal()? {
                Expr::Literal(literal) => Ok(Literal(literal)),
                Expr::TemplateString(template) => Ok(Template(template)),
                _ => self.expected("unexpected literal") //TODO make a proposal to make AST more typed
            }
        }
    }

    fn parse_guard(&mut self) -> ParseResult<Option<Expr<'a>>> {
        if self.cursor.advance(blank().then(of_type(If))).is_none() {
            return Ok(None)
        }

        self.expression().map(Some)
    }

    fn parse_body<P>(&mut self, mut parse_arm: P) -> ParseResult<Expr<'a>>
        where P: FnMut(&mut Self) -> ParseResult<Expr<'a>> {
        self.cursor.force(blank().then(of_type(FatArrow)), "missing '=>'")?;
        parse_arm(self)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use lexer::lexer::lex;
    use crate::parse;

    #[test]
    fn parse_simple_match() {
        let ast = parse(lex("\
        match $1 {\
           -e => ()
           \"test $2\" | -g | $USER | 't x' => ()
           * if [ $a == 1 ] => ()
           * => echo $it
        }\
        ")).expect("parse fail");

        assert_eq!(
            ast,
            vec![]
        )
    }
}