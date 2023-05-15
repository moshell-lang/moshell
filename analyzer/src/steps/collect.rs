use crate::engine::Engine;
use crate::importer::ASTImporter;
use crate::name::Name;
use crate::relations::{Relations, SourceObjectId, UnresolvedImport};
use ast::r#use::Import as AstImport;
use ast::{Expr};
use std::collections::HashSet;
use crate::diagnostic::{Diagnostic, ErrorID, Observation};
use crate::environment::Environment;

/// Defines the current state of the tree exploration.
#[derive(Debug, Clone, Copy)]
struct ResolutionState {
    /// The module id that is currently being explored.
    module: SourceObjectId,

    /// Whether the current module accepts imports.
    accept_imports: bool,
}

impl ResolutionState {
    fn new(module: SourceObjectId) -> Self {
        Self {
            module,
            accept_imports: true,
        }
    }
}

struct SymbolCollector {
    visitable: HashSet<Name>,
    diagnostics: Vec<Diagnostic>,
}

/// Explores the entry point and all its recursive dependencies.
///
/// This collects all the symbols that are used, locally or not yet resolved if they are global.
pub fn collect_symbols<'a, 'b>(
    engine: &mut Engine<'a>,
    relations: &mut Relations,
    entry_point: Name,
    importer: &mut impl ASTImporter,
) -> Result<(), Vec<Diagnostic>> {
    // Prevent re-importing the same names.
    let mut visited: HashSet<Name> = HashSet::new();

    let mut diagnostics = Vec::new();

    //Store all names that still needs to be visited.
    let mut visitable: Vec<Name> = Vec::new();
    // Start by importing the entry point.
    visitable.push(entry_point);
    while let Some(name) = visitable.pop() {
        if !visited.insert(name.clone()) {
            continue;
        }
        // Start by parsing the source read from the importer.
        match import_ast(name, importer) {
            Ok((ast, name)) => collect_ast_symbols(ast, engine, name, relations, &mut visitable, &mut diagnostics),
            Err(diagnostic) => diagnostics.push(diagnostic)
        }
    }
    if !diagnostics.is_empty() {
        return Err(diagnostics)
    }
    Ok(())
}


fn collect_ast_symbols<'a>(ast: Expr<'a>,
                           engine: &mut Engine<'a>,
                           module_name: Name,
                           relations: &mut Relations,
                           visitable: &mut Vec<Name>,
                           diagnostics: &mut Vec<Diagnostic>) {
    // Immediately transfer the ownership of the AST to the engine.
    let root_block = engine.take(ast);

    let mut env = Environment::named(module_name);
    let mut state = ResolutionState::new(engine.track(root_block));
    tree_walk(
        engine,
        relations,
        &mut env,
        &mut state,
        visitable,
        root_block,
        diagnostics,
    );
    engine.attach(state.module, env)
}

fn import_ast<'a, 'b>(
    name: Name,
    importer: &'b mut impl ASTImporter,
) -> Result<(Expr<'a>, Name), Diagnostic> {
    let mut parts = name.parts().to_vec();
    while !parts.is_empty() {
        let name = Name::from(parts.clone());
        match importer.import(&name) {
            Some(expr) => return Ok((expr, name)),
            None => {
                parts.pop();
            }
        }
    }

    Err(Diagnostic::error(ErrorID::CannotImport, &format!("Unable to import AST for module {name}")))
}

/// Collects the symbol import and place it as an [UnresolvedImport] in the relations.
fn collect_symbol_import(
    import: &AstImport,
    relative_path: Vec<String>,
    relations: &mut Relations,
    visitable: &mut Vec<Name>,
    mod_id: SourceObjectId,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match import {
        AstImport::Symbol(s) => {
            let mut symbol_name = relative_path;
            symbol_name.extend(s.path.iter().map(|s| s.to_string()));
            symbol_name.push(s.name.to_string());

            let name = Name::from(symbol_name);
            let alias = s.alias.map(|s| s.to_string());

            visitable.push(name.clone());
            let import = UnresolvedImport::Symbol { alias, name };

            relations.add_import(mod_id, import);
        }
        AstImport::AllIn(path, _) => {
            let mut symbol_name = relative_path;
            symbol_name.extend(path.iter().map(|s| s.to_string()));

            let name = Name::from(symbol_name);
            visitable.push(name.clone());
            relations.add_import(mod_id, UnresolvedImport::AllIn(name));
        }

        AstImport::Environment(_, _) => {
            diagnostics.push(Diagnostic::error(ErrorID::UnsupportedFeature, "import of environment variables and commands are not yet supported.")
                .with_observation(Observation::new(import, mod_id)));
        }
        AstImport::List(list) => {
            for list_import in &list.imports {
                //append ImportList's path to current relative path
                let mut relative = relative_path.clone();
                relative.extend(list.path.iter().map(|s| s.to_string()).collect::<Vec<_>>());

                collect_symbol_import(list_import, relative, relations, visitable, mod_id, diagnostics)
            }
        }
    }
}

fn tree_walk(
    engine: &mut Engine,
    relations: &mut Relations,
    env: &mut Environment,
    state: &mut ResolutionState,
    visitable: &mut Vec<Name>,
    expr: &Expr,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match expr {
        Expr::Use(import) => {
            if !state.accept_imports {
                diagnostics.push(Diagnostic::error(ErrorID::UseBetweenExprs, "Unexpected use statement between expressions. use statements can only be declared on top of environment"));
                return;
            }
            collect_symbol_import(
                &import.import,
                Vec::new(),
                relations,
                visitable,
                state.module,
                diagnostics,
            );
            return;
        }
        Expr::VarDeclaration(var) => {
            if let Some(initializer) = &var.initializer {
                tree_walk(engine, relations, env, state, visitable, initializer, diagnostics);
            }
            env.variables.declare_local(var.var.name.to_owned());
        }
        Expr::VarReference(var) => {
            env.variables.identify(state.module, relations, var.name);
        }
        Expr::Literal(_) => {}
        Expr::Block(block) => {
            env.begin_scope();
            for expr in &block.expressions {
                tree_walk(engine, relations, env, state, visitable, expr, diagnostics);
            }
            env.end_scope();
        }
        Expr::If(if_expr) => {
            env.begin_scope();
            tree_walk(engine, relations, env, state, visitable, &if_expr.condition, diagnostics);
            env.end_scope();
            env.begin_scope();
            tree_walk(
                engine,
                relations,
                env,
                state,
                visitable,
                &if_expr.success_branch,
                diagnostics,
            );
            env.end_scope();
            if let Some(else_branch) = &if_expr.fail_branch {
                env.begin_scope();
                tree_walk(engine, relations, env, state, visitable, else_branch, diagnostics);
                env.end_scope();
            }
        }
        _ => todo!("first pass for {:?}", expr),
    };
    state.accept_imports = false;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::importer::StaticImporter;
    use ast::group::Block;
    use ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
    use context::source::Source;
    use parser::parse_trusted;
    use pretty_assertions::assert_eq;

    #[test]
    fn use_between_expressions() {
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut importer =
            StaticImporter::new([(Name::new("test"), Source::unknown("use a; $a; use c; $c"))], parse_trusted);
        let entry_point = Name::new("test");
        let res = collect_symbols(&mut engine, &mut relations, entry_point, &mut importer).expect_err("collection did not raise errors");
        assert_eq!(
            res,
            vec![
                Diagnostic::error(ErrorID::UseBetweenExprs, "Unexpected use statement between expressions. use statements can only be declared on top of environment"),
                Diagnostic::error(ErrorID::CannotImport, "Unable to import AST for module a")
            ]
        )
    }

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
        let mut relations = Relations::default();
        let mut env = Environment::named(Name::new("test"));
        let mut state = ResolutionState::new(engine.track(&expr));
        let mut diagnostics = Vec::new();

        tree_walk(
            &mut engine,
            &mut relations,
            &mut env,
            &mut state,
            &mut Vec::new(),
            &expr,
            &mut diagnostics,
        );
        assert_eq!(diagnostics, vec![]);
        assert_eq!(relations.objects.len(), 0);
    }
}
