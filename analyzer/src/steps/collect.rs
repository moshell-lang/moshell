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

pub struct SymbolCollector<'a, 'e> {
    engine: &'a mut Engine<'e>,
    relations: &'a mut Relations<'e>,
    diagnostics: Vec<Diagnostic>,
}

impl<'a, 'e> SymbolCollector<'a, 'e> {
    /// Explores the entry point and all its recursive dependencies.
    ///
    /// This collects all the symbols that are used, locally or not yet resolved if they are global.
    pub fn collect_symbols(
        engine: &'a mut Engine<'e>,
        relations: &'a mut Relations<'e>,
        entry_point: Name,
        importer: &mut impl ASTImporter<'e>,
    ) -> Result<(), Vec<Diagnostic>> {
        let mut collector = Self::new(engine, relations);
        collector.collect(entry_point, importer);
        if collector.diagnostics.is_empty() {
            return Ok(())
        }
        Err(collector.diagnostics)
    }

    fn new(engine: &'a mut Engine<'e>, relations: &'a mut Relations<'e>) -> Self {
        Self {
            engine,
            relations,
            diagnostics: Vec::new(),
        }
    }

    fn collect(&mut self, entry_point: Name, importer: &mut impl ASTImporter<'e>) {
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
            //try to import the ast, if the importer isn't able to achieve this and returns None,
            //Ignore this ast analysis. It'll be up to the given importer implementation to handle the
            //errors caused by this import request failure
            if let Some((ast, name)) = import_ast(name, importer) {
                self.collect_ast_symbols(ast, name, &mut visitable)
            }
        }
    }


    fn collect_ast_symbols(&mut self,
                           ast: Expr<'e>,
                           module_name: Name,
                           visitable: &mut Vec<Name>) {
        // Immediately transfer the ownership of the AST to the engine.
        let root_block = self.engine.take(ast);

        let mut env = Environment::named(module_name);
        let mut state = ResolutionState::new(self.engine.track(root_block));
        self.tree_walk(
            &mut env,
            &mut state,
            visitable,
            root_block,
        );
        self.engine.attach(state.module, env)
    }


    /// Collects the symbol import and place it as an [UnresolvedImport] in the relations.
    fn collect_symbol_import(
        &mut self,
        import: &AstImport,
        relative_path: Vec<String>,
        visitable: &mut Vec<Name>,
        mod_id: SourceObjectId,
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

                self.relations.add_import(mod_id, import);
            }
            AstImport::AllIn(path, _) => {
                let mut symbol_name = relative_path;
                symbol_name.extend(path.iter().map(|s| s.to_string()));

                let name = Name::from(symbol_name);
                visitable.push(name.clone());
                self.relations.add_import(mod_id, UnresolvedImport::AllIn(name));
            }

            AstImport::Environment(_, _) => {
                let diagnostic = Diagnostic::error(
                    ErrorID::UnsupportedFeature,
                    mod_id,
                    "import of environment variables and commands are not yet supported.",
                )
                    .with_observation(Observation::new(import));

                self.diagnostics.push(diagnostic);
            }
            AstImport::List(list) => {
                for list_import in &list.imports {
                    //append ImportList's path to current relative path
                    let mut relative = relative_path.clone();
                    relative.extend(list.path.iter().map(|s| s.to_string()).collect::<Vec<_>>());

                    self.collect_symbol_import(list_import, relative, visitable, mod_id)
                }
            }
        }
    }


    fn tree_walk(
        &mut self,
        env: &mut Environment<'e>,
        state: &mut ResolutionState,
        visitable: &mut Vec<Name>,
        expr: &'e Expr<'e>,
    ) {
        match expr {
            Expr::Use(import) => {
                if !state.accept_imports {
                    let diagnostic = Diagnostic::error(
                        ErrorID::UseBetweenExprs,
                        state.module,
                        "Unexpected use statement between expressions. use statements can only be declared on top of environment",
                    );
                    self.diagnostics.push(diagnostic);
                    return;
                }
                self.collect_symbol_import(
                    &import.import,
                    Vec::new(),
                    visitable,
                    state.module,
                );
                return;
            }
            Expr::VarDeclaration(var) => {
                if let Some(initializer) = &var.initializer {
                    self.tree_walk(env, state, visitable, initializer);
                }
                env.variables.declare_local(var.var.name.to_owned());
            }
            Expr::VarReference(var) => {
                env.variables.identify(state.module, self.relations, var);
            }
            Expr::Literal(_) => {}
            Expr::Block(block) => {
                env.begin_scope();
                for expr in &block.expressions {
                    self.tree_walk(env, state, visitable, expr);
                }
                env.end_scope();
            }
            Expr::If(if_expr) => {
                env.begin_scope();
                self.tree_walk(env, state, visitable, &if_expr.condition);
                env.end_scope();
                env.begin_scope();
                self.tree_walk(
                    env,
                    state,
                    visitable,
                    &if_expr.success_branch,
                );
                env.end_scope();
                if let Some(else_branch) = &if_expr.fail_branch {
                    env.begin_scope();
                    self.tree_walk(env, state, visitable, else_branch);
                    env.end_scope();
                }
            }
            _ => todo!("first pass for {:?}", expr),
        };
        state.accept_imports = false;
    }
}


fn import_ast<'a, 'b>(
    name: Name,
    importer: &'b mut impl ASTImporter<'a>,
) -> Option<(Expr<'a>, Name)> {
    let mut parts = name.parts().to_vec();
    while !parts.is_empty() {
        let name = Name::from(parts.clone());
        match importer.import(&name) {
            Some(expr) => return Some((expr, name)),
            None => {
                parts.pop();
            }
        }
    }

    None
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
        let res = SymbolCollector::collect_symbols(&mut engine, &mut relations, entry_point, &mut importer)
            .expect_err("collection did not raise errors");
        assert_eq!(
            res,
            vec![
                Diagnostic::error(ErrorID::UseBetweenExprs, SourceObjectId(0), "Unexpected use statement between expressions. use statements can only be declared on top of environment"),
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
        let mut collector = SymbolCollector::new(&mut engine, &mut relations);

        collector.tree_walk(
            &mut env,
            &mut state,
            &mut Vec::new(),
            &expr,
        );
        assert_eq!(collector.diagnostics, vec![]);
        assert_eq!(relations.objects.len(), 0);
    }
}