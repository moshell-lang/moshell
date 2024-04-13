#![allow(dead_code)]
//! The parser crate contains the parser for the Moshell scripting language.

use ast::group::Block;
use ast::Expr;
use context::source::SourceSegmentHolder;
use std::str::FromStr;

use crate::err::{ParseError, ParseReport};
use crate::parser::Parser;

mod aspects;
mod cursor;
pub mod err;
mod moves;
mod parser;
pub mod source;

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub expressions: Vec<Expr>,
}

pub fn parse(src: &str) -> ParseReport {
    Parser::new(src).parse()
}

/// Parses a supposedly valid string expression
pub fn parse_trusted(src: &str) -> Expr {
    let expressions = parse(src).expect("trusted source input contains invalid expressions");
    Expr::Block(Block {
        expressions,
        segment: src.segment(),
    })
}

impl FromStr for Root {
    type Err = Vec<ParseError>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let report = parse(s);
        if report.is_ok() {
            Ok(Root {
                expressions: report.expr,
            })
        } else {
            Err(report.errors)
        }
    }
}
