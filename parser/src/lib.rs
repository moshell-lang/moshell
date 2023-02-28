#![allow(dead_code)]
#![deny(warnings)]

use crate::ast::Expr;
use lexer::token::Token;

use crate::parser::{ParseResult, Parser};
use crate::source::SourceCode;

///! The parser crate contains the parser for the Moshell scripting language.
mod aspects;
pub mod ast;
mod cursor;
mod moves;
mod parser;
pub mod source;

pub fn parse(tokens: Vec<Token>) -> ParseResult<Vec<Expr>> {
    Parser::new(tokens).parse()
}

pub fn parse_source(src: SourceCode) -> ParseResult<Vec<Expr>> {
    Parser::from_source(src).parse()
}
