#![allow(dead_code)]
#![deny(warnings)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;
use analyzer_system::environment::Environment;

use analyzer_system::layers::ModuleLayers;
use analyzer_system::name::Name;
use ast::{AST, Expr};
use ast::function::FunctionDeclaration;
use ast::group::Block;
use ast::r#use::Use;

use crate::define_std::define_ast;

mod define_std;

//playground
pub fn checker(_working_directory: &Path, files: Vec<AST>) -> Result<(), String> {
    let layers = ModuleLayers::rc_new();
    define_ast(&layers);

    for ast in files {
        let module_path = Path::new(&ast.file_path);
        let module_name = module_path.file_name().ok_or("invalid file name")?.to_str().unwrap();

        let env = ModuleLayers::declare_env(&layers, &Name::new(module_name))?;
        let module = Module {
            name: module_name.to_string(),
            scope: resolve_scope(env, ScopeExpr::BlockExpr(ast.root))
        };
    }

    Ok(())
}

fn resolve_scope(scope_env: Rc<RefCell<Environment>>, block: ScopeExpr) -> Scope {
    let mut inner_scopes = Vec::new();
    for expr in block.expressions() {
        match resolve_expr_scope(&scope_env, expr) {
            None => (),
            Some(scope) => inner_scopes.push(scope)
        }
    }

    Scope {
        env: scope_env,
        uses: HashMap::new(),
        inner_scopes,
        block
    }
}

fn resolve_expr_scope<'a>(parent_env: &'a Rc<RefCell<Environment>>, expr: &'a Expr) -> Option<Scope<'a>> {
    match expr {
        Expr::Assign(_) => {}
        Expr::Binary(_) => {}
        Expr::Match(_) => {}
        Expr::Call(_) => {}
        Expr::ProgrammaticCall(_) => {}
        Expr::MethodCall(_) => {}
        Expr::Pipeline(_) => {}
        Expr::Redirected(_) => {}
        Expr::Detached(_) => {}
        Expr::LambdaDef(_) => {}
        Expr::Substitution(_) => {}
        Expr::TemplateString(_) => {}
        Expr::Use(_) => {}
        Expr::Casted(_) => {}
        Expr::Test(_) => {}
        Expr::Not(_) => {}
        Expr::If(_) => {}
        Expr::While(_) => {}
        Expr::Loop(_) => {}
        Expr::For(_) => {}
        Expr::Continue(_) => {}
        Expr::Break(_) => {}
        Expr::Return(_) => {}
        Expr::VarReference(_) => {}
        Expr::VarDeclaration(_) => {}
        Expr::Range(_) => {}
        Expr::FunctionDeclaration(_) => {}
        Expr::Parenthesis(_) => {}
        Expr::Subshell(_) => {}
        Expr::Block(_) => {}
    }
    None
}

struct Module<'a> {
    name: String,
    scope: Scope<'a>
}

struct Scope<'a> {
    env: Rc<RefCell<Environment>>,
    uses: HashMap<Name, Use<'a>>,
    block: ScopeExpr<'a>,
    inner_scopes: Vec<Scope<'a>>
}


enum ScopeExpr<'a> {
    BlockExpr(Block<'a>),
    FunctionDeclarationExpr(FunctionDeclaration<'a>)
}

impl<'a> ScopeExpr<'a> {
    fn expressions(&self) -> impl Iterator<Item=&Expr> {
        match self {
            ScopeExpr::BlockExpr(b) => b.expressions.as_slice(),
            ScopeExpr::FunctionDeclarationExpr(d) => match d.body.deref() {
                Expr::Block(b) => b.expressions.iter(),
                expr => std::iter::once(expr)
            }
        }
    }
}

#[cfg(test)]
mod tests {

}
