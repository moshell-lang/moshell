use crate::engine::Engine;
use crate::import::Importer;
use analyzer_system::environment::Environment;
use analyzer_system::name::Name;
use analyzer_system::resolver::{Resolver, SourceObjectId};
use ast::group::Block;
use ast::r#use::Import as AstImport;
use ast::Expr;
use parser::parse;
use std::collections::HashSet;
use analyzer_system::import::{UnresolvedImport};
use context::source::{Source, SourceSegmentHolder};
use crate::steps::lib::GatherError;

#[derive(Debug, Clone, Copy)]
struct ResolutionState {
    module: SourceObjectId,
}

fn import_source<'a>(name: Name, importer: &mut impl Importer<'a>) -> Result<(Source<'a>, Name), String> {
    let mut parts = name.parts().to_vec();
    while !parts.is_empty() {
        let name = Name::from(parts.clone());
        match importer.import(&name) {
            Ok(source) => return Ok((source, name)),
            Err(_) => { parts.pop(); }
        }
    }
    Err(format!("Could not import {name}"))
}

/// Explores the entry point and all its recursive dependencies.
///
/// This collects all the symbols that are used, locally or not yet resolved if they are global.
pub fn collect_symbols<'a>(
    engine: &mut Engine<'a>,
    resolver: &mut Resolver,
    entry_point: Name,
    importer: &mut impl Importer<'a>,
) -> Result<(), GatherError> {

    // Prevent re-importing the same names.
    let mut visited: HashSet<Name> = HashSet::new();

    //Store all names that still needs to be visited.
    let mut visitable: Vec<Name> = Vec::new();
    // Start by importing the entry point.
    visitable.push(entry_point);
    while let Some(name) = visitable.pop() {
        if !visited.insert(name.clone()) {
            continue;
        }
        // Start by parsing the source read from the importer.
        let (source, name) = import_source(name, importer).map_err(GatherError::Internal)?;
        let report = parse(source);
        if report.is_err() {
            return Err(GatherError::Parse(report.errors));
        }

        // Immediately transfer the ownership of the AST to the engine.
        let root_block = {
            let expr = report.unwrap();
            let root_block = Expr::Block(Block {
                expressions: expr,
                segment: source.segment(),
            });
            engine.take(root_block)
        };

        let mut env = Environment::named(name);
        let state = ResolutionState {
            module: engine.track(&root_block),
        };
        tree_walk(engine, resolver, &mut env, &mut visitable, state, root_block).map_err(GatherError::Internal)?;
        engine.attach(state.module, env).map_err(GatherError::Internal)?
    }
    Ok(())
}


fn collect_import(
    import: &AstImport,
    relative_path: Vec<String>,
    resolver: &mut Resolver,
    visitable: &mut Vec<Name>,
    mod_id: SourceObjectId,
    engine: &Engine,
) -> Result<(), String> {
    match import {
        AstImport::Symbol(s) => {
            let mut symbol_name = relative_path.clone();
            symbol_name.extend(s.path.iter().map(|s| s.to_string()));
            symbol_name.push(s.name.to_string());

            let name = Name::from(symbol_name);
            let alias = s.alias.map(|s| s.to_string());

            visitable.push(name.clone());
            let import = UnresolvedImport::Symbol { alias, name };

            resolver.add_import(mod_id, import);
            Ok(())
        }
        AstImport::AllIn(path, _) => {
            let mut symbol_name = relative_path.clone();
            symbol_name.extend(path.iter().map(|s| s.to_string()));

            let name = Name::from(symbol_name);
            visitable.push(name.clone());
            resolver.add_import(mod_id, UnresolvedImport::AllIn(name));
            Ok(())
        }

        AstImport::Environment(_, _) => {
            Err("import of environment variables and commands are not yet supported.".to_owned())
        }
        AstImport::List(list) => {
            for list_import in &list.imports {
                //append ImportList's path to current relative path
                let mut relative = relative_path.clone();
                relative.extend(list.path.iter().map(|s| s.to_string()).collect::<Vec<_>>());

                collect_import(&list_import, relative, resolver, visitable, mod_id, engine)?
            }
            Ok(())
        }
    }
}

fn tree_walk<'a>(
    engine: &mut Engine<'a>,
    resolver: &mut Resolver,
    env: &mut Environment,
    visitable: &mut Vec<Name>,
    state: ResolutionState,
    expr: &Expr,
) -> Result<(), String> {
    match expr {
        Expr::Use(use_expr) => collect_import(&use_expr.import, Vec::new(), resolver, visitable, state.module, engine),
        Expr::VarDeclaration(var) => {
            env.variables.declare_local(var.var.name.to_owned());
            Ok(())
        }
        Expr::VarReference(var) => {
            env.variables.identify(state.module, resolver, var.name);
            Ok(())
        }
        Expr::Block(block) => {
            env.begin_scope();
            for expr in &block.expressions {
                tree_walk(engine, resolver, env, visitable, state, expr)?;
            }
            env.end_scope();
            Ok(())
        }
        Expr::If(if_expr) => {
            env.begin_scope();
            tree_walk(engine, resolver, env, visitable, state, &if_expr.condition)?;
            env.end_scope();
            env.begin_scope();
            tree_walk(engine, resolver, env, visitable, state, &if_expr.success_branch)?;
            env.end_scope();
            if let Some(else_branch) = &if_expr.fail_branch {
                env.begin_scope();
                tree_walk(engine, resolver, env, visitable, state, else_branch)?;
                env.end_scope();
            }
            Ok(())
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
        let mut engine = Engine::default();
        let mut resolver = Resolver::default();
        let mut env = Environment::named(Name::new("test"));
        let state = ResolutionState {
            module: engine.track(&expr),
        };

        tree_walk(&mut engine, &mut resolver, &mut env, &mut vec![], state, &expr).expect("tree walk");
        assert_eq!(engine.origins.len(), 1);
        assert_eq!(resolver.objects.len(), 0);
    }
}
