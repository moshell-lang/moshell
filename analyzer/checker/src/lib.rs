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
use ast::control_flow::ForKind;
use ast::group::Block;
use ast::r#use::Use;
use ast::range::Iterable;

use crate::define_std::define_ast;

mod define_std;

//playground
pub fn checker(files: Vec<AST>) -> Result<(), String> {
    let layers = ModuleLayers::rc_new();
    define_ast(&layers);

    for ast in files {
        let module_path = Path::new(&ast.file_path);
        let module_name = module_path.file_name().ok_or("invalid file name")?.to_str().unwrap();

        let env = ModuleLayers::declare_env(&layers, &Name::new(module_name))?;
        let _module = Module {
            name: module_name.to_string(),
            scope: resolve_scope(env, ScopeExpr::BlockExpr(ast.root))
        };

        println!("test");
    }

    Ok(())
}

fn resolve_scope(scope_env: Rc<RefCell<Environment>>, scope_expr: ScopeExpr) -> Scope {
    let mut inner_scopes = Vec::new();
    for scope_candidate in direct_scopeable_expressions(scope_expr.clone().into_expr()) {
        let inner_scope_expr = match ScopeExpr::from_expr(&scope_candidate) {
            Some(inner_scope) => inner_scope,
            None => inner_scopes.push(resolve_scope())
        };

        let env = scope_env.borrow().fork("inner_scope").expect("fork didn't work :(");
        let env = Rc::new(RefCell::new(env));

        inner_scopes.push(resolve_scope(env, inner_scope_expr))
    }

    Scope {
        env: scope_env,
        uses: HashMap::new(),
        inner_scopes,
        block: scope_expr
    }
}

fn find_indirect_scope(parent_env: &Rc<RefCell<Environment>>, expr: &Expr) -> Option<Expr> {
    for sub_expr in direct_scopeable_expressions(scope_expr.clone().into_expr()) {

    }
}

//Lists all sub expressions of given expr that can directly or indirectly contain Scoped expressions.
fn direct_scopeable_expressions(expr: Expr) -> Vec<Expr> {
    match expr {
        Expr::Assign(q) => vec![*q.value.clone()],
        Expr::Binary(b) => vec![*b.right, *b.left],
        Expr::Literal(_) => vec![],
        Expr::Match(m) => m.arms.into_iter().map(|arm| arm.body).collect(),
        Expr::Call(c) => c.arguments,
        Expr::ProgrammaticCall(p) => p.arguments,
        Expr::MethodCall(m) => m.arguments,
        Expr::Pipeline(p) => p.commands,
        Expr::Redirected(r) => vec![*r.expr],
        Expr::Detached(d) => vec![*d.underlying],
        Expr::LambdaDef(l) => vec![*l.body],
        Expr::Substitution(s) => s.underlying.expressions,
        Expr::TemplateString(t) => t.parts,
        Expr::Use(_) => vec![],
        Expr::Casted(c) => vec![*c.expr],
        Expr::Test(t) => vec![*t.expression],
        Expr::Not(n) => vec![*n.underlying],
        Expr::If(i) => {
            let mut vec = vec![*i.condition, *i.success_branch];
            if let Some(b) = i.fail_branch {
                vec.push(*b)
            }
            vec
        },
        Expr::While(w) => vec![*w.condition, *w.body],
        Expr::Loop(l) => vec![*l.body],
        Expr::For(f) => {
            let mut vec = match *f.kind {
                ForKind::Range(r) => vec![r.iterable],
                ForKind::Conditional(c) => vec![c.initializer, c.condition, c.update],
            };
            vec.push(*f.body);
            vec
        }
        Expr::Continue(_) => vec![],
        Expr::Break(_) => vec![],
        Expr::Return(_) => vec![],
        Expr::VarReference(_) => vec![],
        Expr::VarDeclaration(v) => v
            .initializer
            .into_iter()
            .map(|e| *e)
            .collect(),
        Expr::Range(r) => match r {
            Iterable::Range(r) => {
                let mut vec = vec![*r.start, *r.end];
                if let Some(step) = r.step {
                    vec.insert(1, *step);
                }
                vec
            },
            Iterable::Files(_) => vec![]
        }
        Expr::FunctionDeclaration(f) => vec![*f.body],
        Expr::Parenthesis(p) => vec![*p.expression],
        Expr::Subshell(s) => s.expressions,
        Expr::Block(b) => b.expressions,
    }
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

#[derive(Clone)]
enum ScopeExpr<'a> {
    BlockExpr(Block<'a>),
    FunctionBody(Expr<'a>)
}

impl<'a> ScopeExpr<'a> {
    fn from_expr(expr: &Expr<'a>) -> Option<ScopeExpr<'a>> {
        match expr {
            Expr::Block(b) => Some(ScopeExpr::BlockExpr(b.clone())),
            Expr::FunctionDeclaration(f) => Some(ScopeExpr::FunctionBody(f.body.deref().clone())),
            _ => None,
        }
    }

    fn into_expr(self) -> Expr<'a> {
        match self {
            ScopeExpr::BlockExpr(b) => Expr::Block(b),
            ScopeExpr::FunctionBody(expr) => expr
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::AST;
    use ast::group::Block;
    use context::source::Source;
    use parser::parse;
    use crate::checker;

    #[test]
    fn playground_test() {
        let src = "\
use std::*
val a = 18 + 2
val b = $a - 10
fun foo[A](param: A) = {
    val a = $a * 2
    val x = { echo hey this is a sample code; $b }
    for i in { val l = List(param); $l }
        echo \"$(echo $x)\"
}

fun bar(a: Str, b: Str) = foo($a + $b)

loop echo spam !!
";
        let report = parse(Source::unknown(src));
        let ast = AST {
            root: Block {
                expressions: report.expect("got syntax errors in source"),
                segment: 0..0
            },
            file_path: "unknown".to_string(),
        };

        checker(vec![ast]).expect("errors");
    }
}
