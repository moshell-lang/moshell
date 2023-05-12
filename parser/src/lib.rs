#![allow(dead_code)]
#![deny(warnings)]

use crate::err::ParseReport;
use ast::Expr;

use crate::parser::Parser;
use ast::group::Block;
use context::source::{Source, SourceSegmentHolder};

///! The parser crate contains the parser for the Moshell scripting language.
mod aspects;
mod cursor;
pub mod err;
mod moves;
mod parser;
pub mod source;

pub fn parse(src: Source) -> ParseReport {
    Parser::new(src).parse()
}

//parse a supposedly valid string expression
pub fn parse_trusted_str(src: &str) -> Expr {
    let source = Source::unknown(src);
    let expressions = parse(source).expect("trusted str contained invalid expressions");
    Expr::Block(Block {
        expressions,
        segment: source.segment(),
    })
}
