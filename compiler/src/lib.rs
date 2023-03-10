#![allow(dead_code)]

mod analyzer;

use crate::analyzer::{Analyzer, Diagnostic};
use context::source::Source;
use parser::ast::group::Block;
use parser::ast::Expr;
use parser::parse;

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct ValHint {
    pub val_type: ValType,
    pub is_const: bool,
}

impl Default for ValHint {
    fn default() -> Self {
        Self {
            val_type: ValType::Nil,
            is_const: true,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum ValType {
    Bool,
    ExitCode,
    Int,
    Float,
    Any,
    Nil,
}

impl<'a> TryFrom<&'a str> for ValType {
    type Error = String;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        match value {
            "bool" => Ok(ValType::Bool),
            "exitcode" => Ok(ValType::ExitCode),
            "int" => Ok(ValType::Int),
            "float" => Ok(ValType::Float),
            "any" => Ok(ValType::Any),
            "nil" => Ok(ValType::Nil),
            _ => Err(format!("Unknown type: {value}")),
        }
    }
}

pub fn analyze(source: Source) -> Result<ValHint, Vec<Diagnostic>> {
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
