#![allow(dead_code)]
#![deny(warnings)]

use crate::ast::Expr;

use crate::parser::{ParseResult, Parser};
use crate::source::Source;

///! The parser crate contains the parser for the Moshell scripting language.
mod aspects;
pub mod ast;
mod cursor;
mod moves;
mod parser;
pub mod source;

pub fn parse(src: Source) -> ParseResult<Vec<Expr>> {
    Parser::new(src).parse()
}
