#![allow(dead_code)]
#![deny(warnings)]

use crate::diagnostic::ParseReport;
use context::source::Source;

use crate::parser::Parser;

///! The parser crate contains the parser for the Moshell scripting language.
mod aspects;
mod cursor;
pub mod diagnostic;
mod moves;
mod parser;

pub fn parse(src: Source) -> ParseReport {
    Parser::new(src).parse()
}
