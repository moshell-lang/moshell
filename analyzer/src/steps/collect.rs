use std::collections::HashSet;

use ast::call::Call;
use ast::control_flow::ForKind;
use ast::function::FunctionParameter;
use ast::r#match::MatchPattern;
use ast::r#use::Import as ImportExpr;
use ast::range;
use ast::value::LiteralValue;
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};
use range::Iterable;

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::environment::variables::{TypeInfo, Variables};
use crate::environment::Environment;
use crate::importer::ASTImporter;
use crate::imports::{Imports, UnresolvedImport};
use crate::name::Name;
use crate::relations::{RelationState, Relations, SourceId, Symbol};
use crate::steps::resolve::SymbolResolver;
use crate::steps::shared_diagnostics::diagnose_invalid_symbol;

/// Defines the current state of the tree exploration.
#[derive(Debug, Clone, Copy)]
struct ResolutionState {
    /// The module id that is currently being explored.
    module: SourceId,

    /// Whether the current module accepts imports.
    accept_imports: bool,
}

impl ResolutionState {
    fn new(module: SourceId) -> Self {
        Self {
            module,
            accept_imports: true,
        }
    }
}

pub struct SymbolCollector<'a, 'e> {
    engine: &'a mut Engine<'e>,
    relations: &'a mut Relations,
    imports: &'a mut Imports,
    diagnostics: Vec<Diagnostic>,

    /// During the exploration, a parent environment always leaves a reference for its children.
    stack: Vec<(SourceId, &'e Environment)>,
}

impl<'a, 'e> SymbolCollector<'a, 'e> {
    /// Explores the entry point and all its recursive dependencies.
    ///
    /// This collects all the symbols that are used, locally or not yet resolved if they are global.
    /// Returns a vector of diagnostics raised by the collection process.
    pub fn collect_symbols(
        engine: &'a mut Engine<'e>,
        relations: &'a mut Relations,
        imports: &'a mut Imports,
        to_visit: &mut Vec<Name>,
        visited: &mut HashSet<Name>,
        importer: &mut impl ASTImporter<'e>,
    ) -> Vec<Diagnostic> {
        let mut collector = Self::new(engine, relations, imports);
        collector.collect(importer, to_visit, visited);
        collector.check_symbols_identity();
        collector.diagnostics
    }

    fn new(
        engine: &'a mut Engine<'e>,
        relations: &'a mut Relations,
        imports: &'a mut Imports,
    ) -> Self {
        Self {
            engine,
            relations,
            imports,
            diagnostics: Vec::new(),
            stack: Vec::new(),
        }
    }

    /// Performs a check over the collected symbols of root environments
    /// to ensure that the environment does not declares a symbols with the same name of
    /// another module.
    ///
    /// For example, if the module `a` defines a symbol `b`, and the module `a::b` also exists
    /// there is no way to identify if either `a::b` is the symbol, or `a::b` is the module.
    fn check_symbols_identity(&mut self) {
        let roots = self
            .engine
            .environments()
            .filter(|(_, e)| e.parent.is_none()); //keep root environments
        for (env_id, env) in roots {
            let env_name = &env.fqn;
            let mut reported = HashSet::new();
            for (declaration_segment, symbol) in &env.definitions {
                let id = match symbol {
                    Symbol::Local(id) => id,
                    Symbol::External(_) => continue, //we check declarations only, thus external symbols are ignored
                };
                if !reported.insert(id) {
                    continue;
                }
                let var = env
                    .variables
                    .get_var(*id)
                    .expect("local symbol references an unknown variable");
                let var_fqn = env_name.appended(Name::new(&var.name));

                let clashed_module = self
                    .engine
                    .environments()
                    .find(|(_, e)| e.parent.is_none() && e.fqn == var_fqn)
                    .map(|(_, e)| e);

                if let Some(clashed_module) = clashed_module {
                    let inner_modules = {
                        //we know that the inner envs contains at least one environment (the env being clashed with)
                        let list = list_inner_modules(self.engine, &env.fqn)
                            .map(|e| e.fqn.simple_name())
                            .collect::<Vec<_>>();

                        let (head, tail) = list.split_first().unwrap();
                        let str = tail
                            .iter()
                            .fold(format!("{env_name}::{{{head}"), |acc, it| {
                                format!("{acc}, {it}")
                            });
                        format!("{str}}}")
                    };

                    let msg = format!(
                        "Declared symbol '{}' in module {env_name} clashes with module {}",
                        var.name, &clashed_module.fqn
                    );
                    let diagnostic = {
                        Diagnostic::new(DiagnosticID::SymbolConflictsWithModule, env_id, msg)
                            .with_observation(Observation::new(declaration_segment.clone()).with_help(format!("This symbol has the same fully-qualified name as module {}", clashed_module.fqn)))
                            .with_help(format!("You should refactor this symbol with a name that does not conflicts with following modules: {inner_modules}"))
                    };
                    self.diagnostics.push(diagnostic)
                }
            }
        }
    }

    fn collect(
        &mut self,
        importer: &mut impl ASTImporter<'e>,
        to_visit: &mut Vec<Name>,
        visited: &mut HashSet<Name>,
    ) {
        while let Some(name) = to_visit.pop() {
            if !visited.insert(name.clone()) {
                continue;
            }
            //try to import the ast, if the importer isn't able to achieve this and returns None,
            //Ignore this ast analysis. It'll be up to the given importer implementation to handle the
            //errors caused by this import request failure
            if let Some((ast, name)) = import_ast(name, importer) {
                self.collect_ast_symbols(ast, name, to_visit)
            }
        }
    }

    fn collect_ast_symbols(&mut self, ast: Expr<'e>, module_name: Name, to_visit: &mut Vec<Name>) {
        // Immediately transfer the ownership of the AST to the engine.
        let root_block = self.engine.take(ast);

        let mut env = Environment::named(module_name);
        let mut state = ResolutionState::new(self.engine.track(root_block));
        self.tree_walk(&mut env, &mut state, root_block, to_visit);
        self.engine.attach(state.module, env)
    }

    fn add_checked_import(
        &mut self,
        mod_id: SourceId,
        import: UnresolvedImport,
        import_expr: &'e ImportExpr<'e>,
        import_fqn: Name,
    ) {
        if let Some(shadowed) =
            self.imports
                .add_unresolved_import(mod_id, import, import_expr.segment())
        {
            let diagnostic = Diagnostic::new(
                DiagnosticID::ShadowedImport,
                mod_id,
                format!("{import_fqn} is imported twice."),
            )
            .with_observation(Observation::new(shadowed).with_help("useless import here"))
            .with_observation(
                Observation::new(import_expr.segment())
                    .with_help("This statement shadows previous import"),
            );
            self.diagnostics.push(diagnostic)
        }
    }

    /// Collects the symbol import and place it as an [UnresolvedImport] in the relations.
    fn collect_symbol_import(
        &mut self,
        import: &'e ImportExpr<'e>,
        relative_path: Vec<String>,
        mod_id: SourceId,
        to_visit: &mut Vec<Name>,
    ) {
        match import {
            ImportExpr::Symbol(s) => {
                let mut symbol_name = relative_path;
                symbol_name.extend(s.path.iter().map(|s| s.to_string()));
                symbol_name.push(s.name.to_string());

                let name = Name::from(symbol_name);
                let alias = s.alias.map(|s| s.to_string());

                to_visit.push(name.clone());
                let unresolved = UnresolvedImport::Symbol {
                    alias,
                    qualified_name: name.clone(),
                };
                self.add_checked_import(mod_id, unresolved, import, name)
            }
            ImportExpr::AllIn(path, _) => {
                let mut symbol_name = relative_path;
                symbol_name.extend(path.iter().map(|s| s.to_string()));

                let name = Name::from(symbol_name);
                to_visit.push(name.clone());
                let unresolved = UnresolvedImport::AllIn(name.clone());
                self.add_checked_import(mod_id, unresolved, import, name)
            }

            ImportExpr::Environment(_, _) => {
                let diagnostic = Diagnostic::new(
                    DiagnosticID::UnsupportedFeature,
                    mod_id,
                    "import of environment variables and commands are not yet supported.",
                )
                .with_observation(Observation::new(import.segment()));

                self.diagnostics.push(diagnostic);
            }
            ImportExpr::List(list) => {
                for list_import in &list.imports {
                    //append ImportList's path to current relative path
                    let mut relative = relative_path.clone();
                    relative.extend(list.path.iter().map(|s| s.to_string()).collect::<Vec<_>>());

                    self.collect_symbol_import(list_import, relative, mod_id, to_visit)
                }
            }
        }
    }

    /// Identifies a variable [Symbol] from given [Variables] structure.
    /// Will return [Symbol::Local] if the given name isn't qualified and matches
    fn identify_variable(
        &mut self,
        variables: &mut Variables,
        origin: SourceId,
        name: &Name,
        segment: SourceSegment,
    ) -> Symbol {
        macro_rules! track_global {
            () => {
                *variables
                    .external(name.clone())
                    .or_insert_with(|| self.relations.track_new_object(origin))
            };
        }

        match variables.find_reachable(name.root()) {
            None => Symbol::External(track_global!()),
            Some(id) if name.is_qualified() => {
                let var = variables.get_var(id).unwrap();
                self.diagnostics
                    .push(diagnose_invalid_symbol(var.ty, origin, name, &[segment]));
                // instantly declare a dead resolution object
                // We could have returned None here to ignore the symbol but it's more appropriate to
                // bind the variable occurrence with a dead object to signify that it's bound symbol invalid.
                let id = track_global!();
                self.relations[id].state = RelationState::Dead;
                Symbol::External(id)
            }
            Some(id) => Symbol::Local(id),
        }
    }

    fn tree_walk(
        &mut self,
        env: &mut Environment,
        state: &mut ResolutionState,
        expr: &'e Expr<'e>,
        to_visit: &mut Vec<Name>,
    ) {
        match expr {
            Expr::Use(import) => {
                if !state.accept_imports {
                    let diagnostic = Diagnostic::new(
                        DiagnosticID::UseBetweenExprs,
                        state.module,
                        "Unexpected use statement between expressions. use statements can only be declared on top of environment",
                    );
                    self.diagnostics.push(diagnostic);
                    return;
                }
                self.collect_symbol_import(&import.import, Vec::new(), state.module, to_visit);
                return;
            }
            Expr::Assign(assign) => {
                self.tree_walk(env, state, &assign.value, to_visit);
            }
            Expr::Binary(binary) => {
                self.tree_walk(env, state, &binary.left, to_visit);
                self.tree_walk(env, state, &binary.right, to_visit);
            }
            Expr::Match(match_expr) => {
                self.tree_walk(env, state, &match_expr.operand, to_visit);
                for arm in &match_expr.arms {
                    for pattern in &arm.patterns {
                        match pattern {
                            MatchPattern::VarRef(reference) => {
                                let symbol = self.identify_variable(
                                    &mut env.variables,
                                    state.module,
                                    &Name::new(reference.name),
                                    reference.segment(),
                                );
                                env.annotate(reference, symbol);
                            }
                            MatchPattern::Template(template) => {
                                for part in &template.parts {
                                    self.tree_walk(env, state, part, to_visit);
                                }
                            }
                            MatchPattern::Literal(_) | MatchPattern::Wildcard(_) => {}
                        }
                    }
                    if let Some(guard) = &arm.guard {
                        env.begin_scope();
                        self.tree_walk(env, state, guard, to_visit);
                        env.end_scope();
                    }
                    env.begin_scope();
                    if let Some(name) = arm.val_name {
                        env.variables
                            .declare_local(name.to_owned(), TypeInfo::Variable);
                    }
                    self.tree_walk(env, state, &arm.body, to_visit);
                    env.end_scope();
                }
            }
            Expr::Call(call) => {
                self.resolve_primitive_call(env, call);
                for arg in &call.arguments {
                    self.tree_walk(env, state, arg, to_visit);
                }
            }
            Expr::ProgrammaticCall(call) => {
                let path = call
                    .path
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>();
                let name = Name::qualified(path, call.name.to_string());

                let symbol =
                    self.identify_variable(&mut env.variables, state.module, &name, call.segment());

                env.annotate(call, symbol);
                for arg in &call.arguments {
                    self.tree_walk(env, state, arg, to_visit);
                }
            }
            Expr::MethodCall(call) => {
                self.tree_walk(env, state, &call.source, to_visit);
                for arg in &call.arguments {
                    self.tree_walk(env, state, arg, to_visit);
                }
            }
            Expr::Pipeline(pipeline) => {
                for expr in &pipeline.commands {
                    self.tree_walk(env, state, expr, to_visit);
                }
            }
            Expr::Redirected(redirected) => {
                self.tree_walk(env, state, &redirected.expr, to_visit);
                for redir in &redirected.redirections {
                    self.tree_walk(env, state, &redir.operand, to_visit);
                }
            }
            Expr::Detached(detached) => {
                self.tree_walk(env, state, &detached.underlying, to_visit);
            }
            Expr::VarDeclaration(var) => {
                if let Some(initializer) = &var.initializer {
                    self.tree_walk(env, state, initializer, to_visit);
                }
                let symbol = env
                    .variables
                    .declare_local(var.var.name.to_owned(), TypeInfo::Variable);
                env.annotate(var, symbol);
            }
            Expr::VarReference(var) => {
                let symbol = self.identify_variable(
                    &mut env.variables,
                    state.module,
                    &Name::new(var.name),
                    var.segment(),
                );
                env.annotate(var, symbol);
            }
            Expr::Range(range) => match range {
                Iterable::Range(range) => {
                    self.tree_walk(env, state, &range.start, to_visit);
                    self.tree_walk(env, state, &range.end, to_visit);
                }
                Iterable::Files(_) => {}
            },
            Expr::Substitution(sub) => {
                env.begin_scope();
                for expr in &sub.underlying.expressions {
                    self.tree_walk(env, state, expr, to_visit);
                }
                env.end_scope();
            }
            Expr::TemplateString(template) => {
                for expr in &template.parts {
                    self.tree_walk(env, state, expr, to_visit);
                }
            }
            Expr::Casted(casted) => {
                self.tree_walk(env, state, &casted.expr, to_visit);
            }
            Expr::Test(test) => {
                self.tree_walk(env, state, &test.expression, to_visit);
            }
            Expr::Not(not) => {
                self.tree_walk(env, state, &not.underlying, to_visit);
            }
            Expr::Parenthesis(paren) => {
                self.tree_walk(env, state, &paren.expression, to_visit);
            }
            Expr::Subshell(subshell) => {
                env.begin_scope();
                for expr in &subshell.expressions {
                    self.tree_walk(env, state, expr, to_visit);
                }
                env.end_scope();
            }
            Expr::Block(block) => {
                env.begin_scope();
                for expr in &block.expressions {
                    self.tree_walk(env, state, expr, to_visit);
                }
                env.end_scope();
            }
            Expr::If(if_expr) => {
                env.begin_scope();
                self.tree_walk(env, state, &if_expr.condition, to_visit);
                env.end_scope();
                env.begin_scope();
                self.tree_walk(env, state, &if_expr.success_branch, to_visit);
                env.end_scope();
                if let Some(else_branch) = &if_expr.fail_branch {
                    env.begin_scope();
                    self.tree_walk(env, state, else_branch, to_visit);
                    env.end_scope();
                }
            }
            Expr::While(wh) => {
                env.begin_scope();
                self.tree_walk(env, state, &wh.condition, to_visit);
                env.end_scope();
                env.begin_scope();
                self.tree_walk(env, state, &wh.body, to_visit);
                env.end_scope();
            }
            Expr::Loop(lp) => {
                env.begin_scope();
                self.tree_walk(env, state, &lp.body, to_visit);
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
                        self.tree_walk(env, state, &range.iterable, to_visit);
                    }
                    ForKind::Conditional(cond) => {
                        self.tree_walk(env, state, &cond.initializer, to_visit);
                        self.tree_walk(env, state, &cond.condition, to_visit);
                        self.tree_walk(env, state, &cond.increment, to_visit);
                    }
                }
                self.tree_walk(env, state, &fr.body, to_visit);
                env.end_scope();
            }
            Expr::Return(ret) => {
                if let Some(expr) = &ret.expr {
                    self.tree_walk(env, state, expr, to_visit);
                }
            }
            Expr::FunctionDeclaration(func) => {
                let symbol = env
                    .variables
                    .declare_local(func.name.to_owned(), TypeInfo::Function);
                env.annotate(func, symbol);
                let func_id = self.engine.track(expr);
                env.bind_source(func, func_id);
                let mut func_env = env.fork(state.module, func.name);
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
                    &func.body,
                    to_visit,
                );
                self.resolve_captures(env, state, &func_env, func_id);
                self.engine.attach(func_id, func_env);
            }
            Expr::LambdaDef(lambda) => {
                let func_id = self.engine.track(expr);
                let mut func_env = env.fork(state.module, &format!("lambda@{}", func_id.0));
                for param in &lambda.args {
                    let symbol = func_env
                        .variables
                        .declare_local(param.name.to_owned(), TypeInfo::Variable);
                    func_env.annotate(param, symbol);
                }
                self.tree_walk(
                    &mut func_env,
                    &mut ResolutionState::new(func_id),
                    &lambda.body,
                    to_visit,
                );
                self.resolve_captures(env, state, &func_env, func_id);
                self.engine.attach(func_id, func_env);
            }
            Expr::Literal(_) | Expr::Continue(_) | Expr::Break(_) => {}
        }
        state.accept_imports = false;
    }

    fn resolve_captures(
        &mut self,
        parent_env: &Environment,
        parent_state: &ResolutionState,
        capture_env: &Environment,
        capture_env_id: SourceId,
    ) {
        self.stack.push((parent_state.module, unsafe {
            // SAFETY: the reference will immediately be popped off the stack.
            std::mem::transmute::<&Environment, &'e Environment>(parent_env)
        }));
        SymbolResolver::resolve_captures(
            &self.stack,
            self.relations,
            capture_env,
            capture_env_id,
            &mut self.diagnostics,
        );
        self.stack.pop();
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

    fn resolve_primitive_call(&self, env: &mut Environment, call: &Call) -> Option<()> {
        if !call.path.is_empty() {
            return None;
        }
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
    let mut parts = name.into_vec();
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

/// Lists all modules directly contained in the given module name.
fn list_inner_modules<'a>(
    engine: &'a Engine,
    module_fqn: &'a Name,
) -> impl Iterator<Item = &'a Environment> {
    engine
        .environments()
        .filter(move |(_, e)| {
            e.parent.is_none() && e.fqn.tail().filter(|tail| tail == module_fqn).is_some()
        })
        .map(|(_, e)| e)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use context::source::Source;
    use context::str_find::{find_in, find_in_nth};
    use parser::parse_trusted;

    use crate::importer::StaticImporter;
    use crate::relations::{LocalId, RelationId, Symbol};

    use super::*;

    #[test]
    fn use_between_expressions() {
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut importer = StaticImporter::new(
            [(Name::new("test"), Source::unknown("use a; $a; use c; $c"))],
            parse_trusted,
        );
        let res = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &mut vec![Name::new("test")],
            &mut HashSet::new(),
            &mut importer,
        );
        assert_eq!(
            res,
            vec![
                Diagnostic::new(DiagnosticID::UseBetweenExprs, SourceId(0), "Unexpected use statement between expressions. use statements can only be declared on top of environment"),
            ]
        )
    }

    #[test]
    fn bind_local_variables() {
        let expr = parse_trusted(Source::unknown("var bar = 4; $bar"));
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut env = Environment::named(Name::new("test"));
        let mut state = ResolutionState::new(engine.track(&expr));

        let mut collector = SymbolCollector::new(&mut engine, &mut relations, &mut imports);

        collector.tree_walk(&mut env, &mut state, &expr, &mut vec![]);
        assert_eq!(collector.diagnostics, vec![]);
        assert_eq!(relations.iter().collect::<Vec<_>>(), vec![]);
    }

    #[test]
    fn test_symbol_clashes_with_module() {
        let math_source = "use math::{add, multiply, divide}; fun multiply(a: Int, b: Int) = a * b";
        let math_src = Source::unknown(math_source);
        let math_multiply_src = Source::unknown("");
        let math_add_src = Source::unknown("");
        let math_divide_src = Source::unknown("");

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut importer = StaticImporter::new(
            [
                (Name::new("math"), math_src),
                (Name::new("math::multiply"), math_multiply_src),
                (Name::new("math::add"), math_add_src),
                (Name::new("math::divide"), math_divide_src),
            ],
            parse_trusted,
        );

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &mut vec![Name::new("math")],
            &mut HashSet::new(),
            &mut importer,
        );
        assert_eq!(diagnostics, vec![
            Diagnostic::new(DiagnosticID::SymbolConflictsWithModule, SourceId(0), "Declared symbol 'multiply' in module math clashes with module math::multiply")
                .with_observation(Observation::new(find_in(math_source, "fun multiply(a: Int, b: Int) = a * b")).with_help("This symbol has the same fully-qualified name as module math::multiply"))
                .with_help("You should refactor this symbol with a name that does not conflicts with following modules: math::{divide, multiply, add}")
        ]);
    }

    #[test]
    fn shadowed_imports() {
        let source = "use A; use B; use A; use B";
        let test_src = Source::unknown(source);
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut importer = StaticImporter::new([(Name::new("test"), test_src)], parse_trusted);

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &mut vec![Name::new("test")],
            &mut HashSet::new(),
            &mut importer,
        );

        assert_eq!(
            diagnostics,
            vec![
                Diagnostic::new(
                    DiagnosticID::ShadowedImport,
                    SourceId(0),
                    "A is imported twice."
                )
                .with_observation(
                    Observation::new(find_in(source, "A")).with_help("useless import here")
                )
                .with_observation(
                    Observation::new(find_in_nth(source, "A", 1))
                        .with_help("This statement shadows previous import"),
                ),
                Diagnostic::new(
                    DiagnosticID::ShadowedImport,
                    SourceId(0),
                    "B is imported twice."
                )
                .with_observation(
                    Observation::new(find_in(source, "B")).with_help("useless import here")
                )
                .with_observation(
                    Observation::new(find_in_nth(source, "B", 1))
                        .with_help("This statement shadows previous import")
                ),
            ]
        )
    }

    #[test]
    fn bind_function_param() {
        let src = "fun id(a) = return $a";
        let source = Source::unknown(src);
        let expr = parse_trusted(source);
        let mut engine = Engine::default();
        let mut imports = Imports::default();
        let mut relations = Relations::default();
        let mut env = Environment::named(Name::new("test"));
        let mut state = ResolutionState::new(engine.track(&expr));

        let mut collector = SymbolCollector::new(&mut engine, &mut relations, &mut imports);
        collector.tree_walk(&mut env, &mut state, &expr, &mut vec![]);

        assert_eq!(collector.diagnostics, vec![]);
        assert_eq!(relations.iter().collect::<Vec<_>>(), vec![]);
        assert_eq!(
            env.get_raw_symbol(source.segment()),
            Some(Symbol::Local(LocalId(0)))
        );
        assert_eq!(env.get_raw_symbol(find_in(src, "a")), None);
        assert_eq!(env.get_raw_symbol(find_in(src, "$a")), None);
        let func_env = engine.get_environment(SourceId(1)).unwrap();
        assert_eq!(
            func_env.get_raw_symbol(find_in(src, "a")),
            Some(Symbol::Local(LocalId(0)))
        );
        assert_eq!(
            func_env.get_raw_symbol(find_in(src, "$a")),
            Some(Symbol::Local(LocalId(0)))
        );
    }

    #[test]
    fn bind_primitive() {
        let src = "read foo";
        let source = Source::unknown(src);
        let expr = parse_trusted(source);
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut env = Environment::named(Name::new("test"));
        let mut state = ResolutionState::new(engine.track(&expr));

        let mut collector = SymbolCollector::new(&mut engine, &mut relations, &mut imports);
        collector.tree_walk(&mut env, &mut state, &expr, &mut vec![]);

        assert_eq!(collector.diagnostics, vec![]);
        assert_eq!(relations.iter().collect::<Vec<_>>(), vec![]);
        assert_eq!(env.get_raw_symbol(find_in(src, "read")), None);
        assert_eq!(
            env.get_raw_symbol(find_in(src, "foo")),
            Some(Symbol::Local(LocalId(0)))
        );
    }

    #[test]
    fn find_references() {
        let src = "$bar; baz($foo, $bar)";
        let source = Source::unknown(src);
        let expr = parse_trusted(source);

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut env = Environment::named(Name::new("test"));
        let mut state = ResolutionState::new(engine.track(&expr));

        let mut collector = SymbolCollector::new(&mut engine, &mut relations, &mut imports);
        collector.tree_walk(&mut env, &mut state, &expr, &mut vec![]);

        assert_eq!(collector.diagnostics, vec![]);
        engine.attach(state.module, env);
        assert_eq!(
            relations
                .find_references(&engine, RelationId(0))
                .map(|mut references| {
                    references.sort_by_key(|range| range.start);
                    references
                }),
            Some(vec![find_in(src, "$bar"), find_in_nth(src, "$bar", 1)])
        );
        assert_eq!(
            relations.find_references(&engine, RelationId(1)),
            Some(vec![find_in(src, "baz($foo, $bar)")])
        );
        assert_eq!(
            relations.find_references(&engine, RelationId(2)),
            Some(vec![find_in(src, "$foo")])
        );
    }
}
