#![allow(dead_code)]
#![deny(warnings)]

use crate::err::ParseReport;
use context::source::StringSource;

use crate::parser::Parser;

///! The parser crate contains the parser for the Moshell scripting language.
mod aspects;
pub mod ast;
mod cursor;
pub mod err;
mod moves;
mod parser;

pub fn parse(src: StringSource) -> ParseReport {
    Parser::new(src).parse()
}
