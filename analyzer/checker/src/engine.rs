use analyzer_system::environment::Environment;
use analyzer_system::resolver::GlobalObjectId;
use ast::Expr;
use std::collections::HashMap;

/// Owns references to the global AST and its environments.
#[derive(Debug, Default)]
pub struct Engine<'a> {
    /// The engine has the ownership of the AST.
    asts: Vec<Box<Expr<'a>>>,

    /// Associates a module id to the corresponding environment
    resp: HashMap<GlobalObjectId, (&'a Expr<'a>, Environment)>,
}

impl<'a> Engine<'a> {
    /// Takes ownership of an expression and returns a reference to it.
    pub fn take(&mut self, ast: Expr<'a>) -> &'a Expr<'a> {
        self.asts.push(Box::new(ast));
        unsafe {
            // SAFETY: Assume for now that expressions are never removed from the engine.
            // The reference behind Box does not change and is valid for the lifetime of the engine.
            std::mem::transmute::<&Expr<'a>, &'a Expr<'a>>(self.asts.last().unwrap())
        }
    }

    pub fn track(&mut self, root_id: GlobalObjectId, ast: &'a Expr<'a>, env: Environment) {
        self.resp.insert(root_id, (ast, env));
    }
}
