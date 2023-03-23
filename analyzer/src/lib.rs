#![allow(dead_code)]

mod analyzer;
pub mod builtin_types;
pub mod context;
mod environment;
pub mod types;

use crate::analyzer::{Analyzer, Diagnostic};
use crate::types::Type;
use ::context::source::Source;
use ast::group::Block;
use ast::Expr;
use parser::parse;

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
