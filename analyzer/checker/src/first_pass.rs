use crate::import::Importer;
use analyzer_system::environment::Environment;
use analyzer_system::name::Name;
use analyzer_system::resolver::{GlobalObjectId, Resolver};
use ast::group::Block;
use ast::r#use::Import;
use ast::Expr;
use parser::err::ParseError;
use parser::parse;
use std::collections::HashSet;
use std::io;

#[derive(Debug, Clone, Copy)]
struct ResolutionState {
    module: GlobalObjectId,
}

#[derive(Debug)]
pub enum GatherError {
    Import(io::Error),
    Parse(Vec<ParseError>),
}

impl From<io::Error> for GatherError {
    fn from(err: io::Error) -> Self {
        Self::Import(err)
    }
}

pub fn first_pass<'a>(
    resolver: &mut Resolver,
    entry_point: Name,
    mut importer: impl Importer<'a>,
) -> Result<(), GatherError> {
    resolver.visitable.push(entry_point);
    let mut visited: HashSet<Name> = HashSet::new();
    while let Some(name) = resolver.visitable.pop() {
        if !visited.insert(name.clone()) {
            continue;
        }
        let source = importer.import(&name)?;
        let report = parse(source);
        if report.is_err() {
            return Err(GatherError::Parse(report.errors));
        }

        let expr = report.unwrap();
        let state = ResolutionState {
            module: resolver.track_new_object(),
        };
        tree_walk(
            resolver,
            &mut Environment::named(name),
            state,
            &Expr::Block(Block {
                expressions: expr,
                segment: Default::default(),
            }),
        );
    }
    Ok(())
}

fn tree_walk(resolver: &mut Resolver, env: &mut Environment, state: ResolutionState, expr: &Expr) {
    match expr {
        Expr::Use(use_expr) => match &use_expr.import {
            Import::Symbol(symbol) => {
                let mut name = symbol
                    .path
                    .iter()
                    .copied()
                    .map(ToOwned::to_owned)
                    .collect::<Vec<String>>();
                name.push(symbol.name.to_owned());
                resolver.visitable.push(Name::from(name));
            }
            _ => todo!("first pass for {:?}", expr),
        },
        Expr::VarDeclaration(var) => {
            env.variables.declare_local(var.var.name.to_owned());
        }
        Expr::VarReference(var) => {
            env.variables.identify(resolver, var.name);
        }
        Expr::Block(block) => {
            env.begin_scope();
            for expr in &block.expressions {
                tree_walk(resolver, env, state, expr);
            }
            env.end_scope();
        }
        Expr::If(if_expr) => {
            env.begin_scope();
            tree_walk(resolver, env, state, &if_expr.condition);
            env.end_scope();
            env.begin_scope();
            tree_walk(resolver, env, state, &if_expr.success_branch);
            env.end_scope();
            if let Some(else_branch) = &if_expr.fail_branch {
                env.begin_scope();
                tree_walk(resolver, env, state, else_branch);
                env.end_scope();
            }
        }
        _ => todo!("first pass for {:?}", expr),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::group::Block;
    use ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};

    #[test]
    fn bind_local_variables() {
        let expr = Expr::Block(Block {
            expressions: vec![
                Expr::VarDeclaration(VarDeclaration {
                    kind: VarKind::Var,
                    var: TypedVariable {
                        name: "bar",
                        ty: None,
                        segment: 0..1,
                    },
                    initializer: None,
                    segment: 0..2,
                }),
                Expr::VarReference(VarReference {
                    name: "bar",
                    segment: 0..1,
                }),
            ],
            segment: 0..3,
        });
        let mut resolver = Resolver::default();
        let mut env = Environment::named(Name::new("test"));
        let state = ResolutionState {
            module: resolver.track_new_object(),
        };
        tree_walk(&mut resolver, &mut env, state, &expr);
        assert_eq!(resolver.objects.len(), 1);
    }
}
