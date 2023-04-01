#![allow(dead_code)]

mod analyzer;
pub mod environment;
pub mod lang_types;
pub mod local;
pub mod types;

use crate::analyzer::Analyzer;
use crate::types::types::{Type};
use ::context::source::Source;
use ast::group::Block;
use ast::Expr;
use parser::parse;

#[derive(Debug, PartialEq)]
pub struct Diagnostic {
    pub message: String,
}

pub fn analyze(source: Source) -> Result<Type, Vec<Diagnostic>> {
    let parsed = parse(source.clone()).expect("Failed to parse");
    let mut analyzer = Analyzer::new(source);
    let result = analyzer.analyze_all(&Expr::Block(Block {
        expressions: parsed,
    }));
    if analyzer.diagnostics.is_empty() {
        Ok(result.expect("No result"))
    } else {
        Err(analyzer.diagnostics)
    }
}
