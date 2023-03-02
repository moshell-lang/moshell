#![allow(dead_code)]
#![deny(warnings)]

use crate::err::ParseReport;
use context::source::Source;

use crate::parser::Parser;

///! The parser crate contains the parser for the Moshell scripting language.
mod aspects;
pub mod ast;
mod cursor;
pub mod err;
mod moves;
mod parser;

pub fn parse(src: Source) -> ParseReport {
    Parser::new(src).parse().into()
}
