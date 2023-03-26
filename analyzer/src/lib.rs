#![allow(dead_code)]

mod analyzer;
pub mod builtin_types;
mod classes;
pub mod context;
pub mod environment;
pub mod types;

use crate::analyzer::Analyzer;
use crate::types::{Type, TypeScheme};
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
        Ok(match result.expect("No result") {
            TypeScheme::Monotype(ty) => ty,
            _ => panic!("Unexpected type scheme"),
        })
    } else {
        Err(analyzer.diagnostics)
    }
}
