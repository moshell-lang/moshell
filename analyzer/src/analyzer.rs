
use crate::environment::Environment;
use crate::types::{Type};
use crate::Diagnostic;
use ast::function::FunctionParameter;
use ast::value::LiteralValue;
use ast::Expr;
use context::source::Source;
use crate::builtin_types::{float, int, str, unit};

pub struct Analyzer<'a> {
    pub source: Source<'a>,
    pub diagnostics: Vec<Diagnostic>,
}

impl<'a> Analyzer<'a> {
    pub fn new(source: Source<'a>) -> Self {
        Self {
            source,
            diagnostics: Vec::new(),
        }
    }

    pub fn analyze_all(&mut self, expr: &Expr) -> Option<Type> {
        let mut environment = Environment::lang();
        self.analyze(&mut environment, expr)
    }

    fn analyze(&mut self, environment: &mut Environment, expr: &Expr) -> Option<Type> {
        todo!()
    }
}
