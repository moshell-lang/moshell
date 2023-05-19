use crate::engine::Engine;

use crate::environment::variables::TypeInfo;
use crate::importer::ASTImporter;
use crate::name::Name;
use ast::call::Call;
use ast::control_flow::ForKind;
use ast::function::FunctionParameter;
use ast::r#match::MatchPattern;
use ast::r#use::Import as ImportExpr;
use ast::range::Iterable;
use ast::value::LiteralValue;
use ast::Expr;
use std::collections::HashSet;
use crate::diagnostic::{Diagnostic, ErrorID, Observation, WarnID};
use crate::environment::Environment;
use crate::relations::{Relations, SourceObjectId, UnresolvedImport};

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
    ) -> Vec<Diagnostic> {
        let mut collector = Self::new(engine, relations);
        collector.collect(entry_point, importer);
        collector.diagnostics
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

    fn add_checked_import(&mut self,
                          mod_id: SourceObjectId,
                          import: UnresolvedImport,
                          import_expr: &'e ImportExpr<'e>,
                          import_fqn: Name) {
        if let Some(shadowed) = self.relations.add_import(mod_id, import, import_expr) {
            let diagnostic = Diagnostic::warn(WarnID::ShadowedImport, mod_id, &format!("{import_fqn} is imported twice."))
                .with_observation(Observation::with_help(shadowed, "useless import here"))
                .with_observation(Observation::with_help(import_expr, "This statement shadows previous import"));
            self.diagnostics.push(diagnostic)
        }
    }

    /// Collects the symbol import and place it as an [UnresolvedImport] in the relations.
    fn collect_symbol_import(
        &mut self,
        import: &'e ImportExpr<'e>,
        relative_path: Vec<String>,
        visitable: &mut Vec<Name>,
        mod_id: SourceObjectId,
    ) {
        match import {
            ImportExpr::Symbol(s) => {
                let mut symbol_name = relative_path;
                symbol_name.extend(s.path.iter().map(|s| s.to_string()));
                symbol_name.push(s.name.to_string());

                let name = Name::from(symbol_name);
                let alias = s.alias.map(|s| s.to_string());

                visitable.push(name.clone());
                let unresolved = UnresolvedImport::Symbol { alias, fqn: name.clone() };
                self.add_checked_import(mod_id, unresolved, import, name)
            }
            ImportExpr::AllIn(path, _) => {
                let mut symbol_name = relative_path;
                symbol_name.extend(path.iter().map(|s| s.to_string()));

                let name = Name::from(symbol_name);
                visitable.push(name.clone());
                let unresolved = UnresolvedImport::AllIn(name.clone());
                self.add_checked_import(mod_id, unresolved, import, name)
            }

            ImportExpr::Environment(_, _) => {
                let diagnostic = Diagnostic::error(
                    ErrorID::UnsupportedFeature,
                    mod_id,
                    "import of environment variables and commands are not yet supported.",
                )
                    .with_observation(Observation::new(import));

                self.diagnostics.push(diagnostic);
            }
            ImportExpr::List(list) => {
                for list_import in &list.imports {
                    //append ImportList's path to current relative path
                    let mut relative = relative_path.clone();
                    relative.extend(list.path.iter().map(|s| s.to_string()).collect::<Vec<_>>());

                    self.collect_symbol_import(list_import, relative, visitable, mod_id)
                }
            }
        }
    }

    fn tree_walk(&mut self,
                 env: &mut Environment,
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
            Expr::Assign(assign) => {
                self.tree_walk(env, state, visitable, &assign.value);
            }
            Expr::Binary(binary) => {
                self.tree_walk(env, state, visitable, &binary.left);
                self.tree_walk(env, state, visitable, &binary.right);
            }
            Expr::Match(match_expr) => {
                self.tree_walk(env, state, visitable, &match_expr.operand);
                for arm in &match_expr.arms {
                    for pattern in &arm.patterns {
                        match pattern {
                            MatchPattern::VarRef(reference) => {
                                let symbol =
                                    env.variables.identify(state.module, self.relations, reference.name.to_string());
                                env.annotate(reference, symbol);
                            }
                            MatchPattern::Template(template) => {
                                for part in &template.parts {
                                    self.tree_walk(env, state, visitable, part);
                                }
                            }
                            MatchPattern::Literal(_) | MatchPattern::Wildcard(_) => {}
                        }
                    }
                    if let Some(guard) = &arm.guard {
                        env.begin_scope();
                        self.tree_walk(env, state, visitable, guard);
                        env.end_scope();
                    }
                    env.begin_scope();
                    if let Some(name) = arm.val_name {
                        env.variables
                            .declare_local(name.to_owned(), TypeInfo::Variable);
                    }
                    self.tree_walk(env, state, visitable, &arm.body);
                    env.end_scope();
                }
            }
            Expr::Call(call) => {
                self.resolve_primitive(env, call);
                for arg in &call.arguments {
                    self.tree_walk(env, state, visitable, arg);
                }
            }
            Expr::ProgrammaticCall(call) => {
                let symbol = env.variables.identify(state.module, self.relations, call.name.to_string());
                env.annotate(call, symbol);
                for arg in &call.arguments {
                    self.tree_walk(env, state, visitable, arg);
                }
            }
            Expr::MethodCall(call) => {
                self.tree_walk(env, state, visitable, &call.source);
                for arg in &call.arguments {
                    self.tree_walk(env, state, visitable, arg);
                }
            }
            Expr::Pipeline(pipeline) => {
                for expr in &pipeline.commands {
                    self.tree_walk(env, state, visitable, expr);
                }
            }
            Expr::Redirected(redirected) => {
                self.tree_walk(env, state, visitable, &redirected.expr);
                for redir in &redirected.redirections {
                    self.tree_walk(env, state, visitable, &redir.operand);
                }
            }
            Expr::Detached(detached) => {
                self.tree_walk(
                    env,
                    state,
                    visitable,
                    &detached.underlying,
                );
            }
            Expr::VarDeclaration(var) => {
                if let Some(initializer) = &var.initializer {
                    self.tree_walk(env, state, visitable, initializer);
                }
                let symbol = env
                    .variables
                    .declare_local(var.var.name.to_owned(), TypeInfo::Variable);
                env.annotate(var, symbol);
            }
            Expr::VarReference(var) => {
                let symbol = env.variables.identify(state.module, self.relations, var.name.to_string());
                env.annotate(var, symbol);
            }
            Expr::Range(range) => match range {
                Iterable::Range(range) => {
                    self.tree_walk(env, state, visitable, &range.start);
                    self.tree_walk(env, state, visitable, &range.end);
                }
                Iterable::Files(_) => {}
            },
            Expr::Substitution(sub) => {
                env.begin_scope();
                for expr in &sub.underlying.expressions {
                    self.tree_walk(env, state, visitable, expr);
                }
                env.end_scope();
            }
            Expr::TemplateString(template) => {
                for expr in &template.parts {
                    self.tree_walk(env, state, visitable, expr);
                }
            }
            Expr::Casted(casted) => {
                self.tree_walk(env, state, visitable, &casted.expr);
            }
            Expr::Test(test) => {
                self.tree_walk(env, state, visitable, &test.expression);
            }
            Expr::Not(not) => {
                self.tree_walk(env, state, visitable, &not.underlying);
            }
            Expr::Parenthesis(paren) => {
                self.tree_walk(env, state, visitable, &paren.expression);
            }
            Expr::Subshell(subshell) => {
                env.begin_scope();
                for expr in &subshell.expressions {
                    self.tree_walk(env, state, visitable, expr);
                }
                env.end_scope();
            }
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
            Expr::While(wh) => {
                env.begin_scope();
                self.tree_walk(env, state, visitable, &wh.condition);
                env.end_scope();
                env.begin_scope();
                self.tree_walk(env, state, visitable, &wh.body);
                env.end_scope();
            }
            Expr::Loop(lp) => {
                env.begin_scope();
                self.tree_walk(env, state, visitable, &lp.body);
                env.end_scope();
            }
            Expr::For(fr) => {
                env.begin_scope();
                match fr.kind.as_ref() {
                    ForKind::Range(range) => {
                        let symbol = env
                            .variables
                            .declare_local(range.receiver.to_owned(), TypeInfo::Variable);
                        env.annotate(range, symbol);
                        self.tree_walk(env, state, visitable, &range.iterable);
                    }
                    ForKind::Conditional(cond) => {
                        self.tree_walk(env, state, visitable, &cond.initializer);
                        self.tree_walk(env, state, visitable, &cond.condition);
                        self.tree_walk(env, state, visitable, &cond.increment);
                    }
                }
                self.tree_walk(env, state, visitable, &fr.body);
                env.end_scope();
            }
            Expr::Return(ret) => {
                if let Some(expr) = &ret.expr {
                    self.tree_walk(env, state, visitable, expr);
                }
            }
            Expr::FunctionDeclaration(func) => {
                let symbol = env
                    .variables
                    .declare_local(func.name.to_owned(), TypeInfo::Function);
                env.annotate(func, symbol);
                let func_id = self.engine.track(expr);
                let mut func_env = env.fork(func_id, func.name);
                for param in &func.parameters {
                    let symbol = func_env.variables.declare_local(
                        match param {
                            FunctionParameter::Named(named) => named.name.to_owned(),
                            FunctionParameter::Variadic(_) => "@".to_owned(),
                        },
                        TypeInfo::Variable,
                    );
                    // Only named parameters can be annotated for now
                    if let FunctionParameter::Named(named) = param {
                        func_env.annotate(named, symbol);
                    }
                }
                self.tree_walk(
                    &mut func_env,
                    &mut ResolutionState::new(func_id),
                    visitable,
                    &func.body,
                );
                self.engine.attach(func_id, func_env);
            }
            Expr::LambdaDef(lambda) => {
                let func_id = self.engine.track(expr);
                let mut func_env = env.fork(func_id, &format!("lambda@{}", func_id.0));
                for param in &lambda.args {
                    let symbol = func_env
                        .variables
                        .declare_local(param.name.to_owned(), TypeInfo::Variable);
                    func_env.annotate(param, symbol);
                }
                self.tree_walk(
                    &mut func_env,
                    &mut ResolutionState::new(func_id),
                    visitable,
                    &lambda.body,
                );
                self.engine.attach(func_id, func_env);
            }
            Expr::Literal(_) | Expr::Continue(_) | Expr::Break(_) => {}
        }
        state.accept_imports = false;
    }

    fn extract_literal_argument(&self, call: &'a Call, nth: usize) -> Option<&'a str> {
        match call.arguments.get(nth)? {
            Expr::Literal(lit) => match &lit.parsed {
                LiteralValue::String(str) => Some(str),
                _ => None,
            },
            _ => None,
        }
    }

    fn resolve_primitive(&self, env: &mut Environment, call: &Call) -> Option<()> {
        let command = self.extract_literal_argument(call, 0)?;
        match command {
            "read" => {
                let var = self.extract_literal_argument(call, 1)?;
                let symbol = env
                    .variables
                    .declare_local(var.to_owned(), TypeInfo::Variable);
                env.annotate(&call.arguments[1], symbol);
                Some(())
            }
            _ => None,
        }
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
    use context::source::{Source, StaticSegmentHolder};
    use parser::parse_trusted;
    use pretty_assertions::assert_eq;

    #[test]
    fn use_between_expressions() {
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut importer =
            StaticImporter::new([(Name::new("test"), Source::unknown("use a; $a; use c; $c"))], parse_trusted);
        let entry_point = Name::new("test");
        let res = SymbolCollector::collect_symbols(&mut engine, &mut relations, entry_point, &mut importer);
        assert_eq!(
            res,
            vec![
                Diagnostic::error(ErrorID::UseBetweenExprs, SourceObjectId(0), "Unexpected use statement between expressions. use statements can only be declared on top of environment"),
            ]
        )
    }

    #[test]
    fn bind_local_variables() {
        let expr = parse_trusted(Source::unknown("var bar = 4; $bar"));
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
        assert_eq!(relations.objects, vec![]);
    }

    #[test]
    fn shadowed_imports() {
        let test_src = Source::unknown("use A; use B; use A; use B");
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut importer = StaticImporter::new([(Name::new("test"), test_src)], parse_trusted);
        let diagnostics = SymbolCollector::collect_symbols(&mut engine, &mut relations, Name::new("test"), &mut importer);

        assert_eq!(
            diagnostics,
            vec![
                Diagnostic::warn(WarnID::ShadowedImport, SourceObjectId(0), "A is imported twice.")
                    .with_observation(Observation::with_help(&StaticSegmentHolder::new(4..5), "useless import here"))
                    .with_observation(Observation::with_help(&StaticSegmentHolder::new(18..19), "This statement shadows previous import")),
                Diagnostic::warn(WarnID::ShadowedImport, SourceObjectId(0), "B is imported twice.")
                    .with_observation(Observation::with_help(&StaticSegmentHolder::new(11..12), "useless import here"))
                    .with_observation(Observation::with_help(&StaticSegmentHolder::new(25..26), "This statement shadows previous import")),
            ]
        )
    }

}