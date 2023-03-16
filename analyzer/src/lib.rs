#![allow(dead_code)]

mod analyzer;
mod environment;
pub mod type_scheme;

use crate::analyzer::{Analyzer, Diagnostic};
use crate::type_scheme::TypeScheme;
use ast::group::Block;
use ast::Expr;
use context::source::Source;
use parser::parse;

pub fn analyze(source: Source) -> Result<TypeScheme, Vec<Diagnostic>> {
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
