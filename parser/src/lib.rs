#![allow(dead_code)]

use crate::ast::Expr;
use lexer::token::Token;

use crate::parser::{ParseResult, Parser};

///! The parser crate contains the parser for the Moshell scripting language.
mod aspects;
pub mod ast;
mod cursor;
mod moves;
mod parser;

pub fn parse(tokens: Vec<Token>) -> ParseResult<Vec<Expr>> {
    Parser::new(tokens).parse()
}
