#![allow(dead_code)]
#![deny(warnings)]

use crate::err::ParseReport;
use lexer::reader::BufferedTokenReader;

use crate::parser::Parser;

///! The parser crate contains the parser for the Moshell scripting language.
mod aspects;
pub mod ast;
mod cursor;
pub mod err;
mod moves;
mod parser;

pub fn parse<S>(reader: BufferedTokenReader<S>) -> ParseReport {
    Parser::new(reader).parse()
}
