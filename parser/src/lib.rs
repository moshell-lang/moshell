#![allow(dead_code)]
//! The parser crate contains the parser for the Moshell scripting language.

use ast::group::Block;
use ast::Expr;
use context::source::{Source, SourceSegmentHolder};

use crate::err::ParseReport;
use crate::parser::Parser;

mod aspects;
mod cursor;
pub mod err;
mod moves;
mod parser;
pub mod source;

pub fn parse(src: Source) -> ParseReport {
    Parser::new(src).parse()
}

/// Parses a supposedly valid string expression
pub fn parse_trusted(src: Source) -> Expr {
    let expressions = parse(src).expect("trusted str contained invalid expressions");
    Expr::Block(Block {
        expressions,
        segment: src.segment(),
    })
}
