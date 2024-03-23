use std::collections::HashSet;

use ast::call::Call;
use ast::control_flow::ForKind;
use ast::function::FunctionParameter;
use ast::r#match::MatchPattern;
use ast::r#type::Type;
use ast::r#use::{Import as ImportExpr, InclusionPathItem};
use ast::range;
use ast::value::LiteralValue;
use ast::variable::{Tilde, VarName};
use ast::Expr;
use context::source::{ContentId, SourceSegment, SourceSegmentHolder};
use range::Iterable;

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::environment::symbols::{MagicSymbolKind, SymbolInfo, SymbolLocation, SymbolRegistry};
use crate::environment::Environment;
use crate::importer::{ASTImporter, ImportResult, Imported};
use crate::imports::{Imports, UnresolvedImport};
use crate::name::Name;
use crate::reef::{Externals, ReefId};
use crate::relations::{RelationState, Relations, SourceId, SymbolRef};
use crate::steps::resolve::SymbolResolver;
use crate::steps::shared_diagnostics::diagnose_invalid_symbol;
use crate::steps::typing::magic::is_magic_variable_name;
use crate::Inject;

/// Defines the current state of the tree exploration.
#[derive(Debug, Clone, Copy)]
struct ResolutionState {
    /// The id of the AST tree that is currently being explored.
    content: ContentId,

    /// The module id that is currently being explored.
    module: SourceId,

    /// Whether the current module accepts imports.
    accept_imports: bool,
}

impl ResolutionState {
    fn new(content: ContentId, module: SourceId) -> Self {
        Self {
            content,
            module,
            accept_imports: true,
        }
    }

    fn fork(self, module: SourceId) -> Self {
        Self {
            content: self.content,
            module,
            accept_imports: false,
        }
    }
}

pub struct SymbolCollector<'a, 'b, 'e> {
    engine: &'a mut Engine<'e>,
    relations: &'a mut Relations,
    imports: &'a mut Imports,
    externals: &'b Externals<'b>,
    diagnostics: Vec<Diagnostic>,

    /// The stack of environments currently being collected.
    stack: Vec<SourceId>,
}

impl<'a, 'b, 'e> SymbolCollector<'a, 'b, 'e> {
    /// Explores the entry point and all its recursive dependencies.
    ///
    /// This collects all the symbols that are used, locally or not yet resolved if they are global.
    /// Returns a vector of diagnostics raised by the collection process.
    pub fn collect_symbols(
        engine: &'a mut Engine<'e>,
        relations: &'a mut Relations,
        imports: &'a mut Imports,
        externals: &'b Externals<'b>,
        to_visit: &mut Vec<Name>,
        visited: &mut HashSet<Name>,
        importer: &mut impl ASTImporter,
    ) -> Vec<Diagnostic> {
        let mut collector = Self::new(engine, relations, imports, externals);
        collector.collect(importer, to_visit, visited);
        collector.check_symbols_identity();
        collector.diagnostics
    }

    pub fn inject(
        inject: Inject,
        engine: &'a mut Engine<'e>,
        relations: &'a mut Relations,
        imports: &'a mut Imports,
        externals: &'b Externals<'b>,
        to_visit: &mut Vec<Name>,
    ) -> Vec<Diagnostic> {
        assert_ne!(
            inject.attached,
            Some(SourceId(engine.len())),
            "Cannot inject a module to itself"
        );
        let mut collector = Self::new(engine, relations, imports, externals);
        let root_block = collector.engine.take(inject.imported.expr);

        let mut env = Environment::script(inject.name);
        env.parent = inject.attached;
        let mut state = ResolutionState::new(
            inject.imported.content,
            collector.engine.track(inject.imported.content, root_block),
        );
        collector.engine.attach(state.module, env);
        collector.stack.push(state.module);

        collector.tree_walk(&mut state, root_block, to_visit);
        collector.stack.pop();
        collector.check_symbols_identity();
        collector.diagnostics
    }

    fn new(
        engine: &'a mut Engine<'e>,
        relations: &'a mut Relations,
        imports: &'a mut Imports,
        externals: &'b Externals<'b>,
    ) -> Self {
        Self {
            engine,
            relations,
            imports,
            externals,
            diagnostics: Vec::new(),
            stack: Vec::new(),
        }
    }

    fn current_env(&mut self) -> &mut Environment {
        self.engine
            .get_environment_mut(*self.stack.last().unwrap())
            .unwrap()
    }

    fn engine(&mut self) -> &mut Engine<'e> {
        self.engine
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
                    SymbolRef::Local(id) => id,
                    SymbolRef::External(_) => continue, //we check declarations only, thus external symbols are ignored
                };
                if !reported.insert(id) {
                    continue;
                }
                let symbol = env
                    .symbols
                    .get(*id)
                    .expect("local symbol references an unknown variable");
                let var_fqn = env_name.appended(Name::new(&symbol.name));

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
                        symbol.name, &clashed_module.fqn
                    );
                    let diagnostic = {
                        Diagnostic::new(DiagnosticID::SymbolConflictsWithModule, msg)
                            .with_observation(
                                Observation::here(
                                    env_id,
                                    self.externals.current,
                                    declaration_segment.clone(),
                                    format!("This symbol has the same fully-qualified name as module {}", clashed_module.fqn)
                                )
                            )
                            .with_help(format!("You should refactor this symbol with a name that does not conflicts with following modules: {inner_modules}"))
                    };
                    self.diagnostics.push(diagnostic)
                }
            }
        }
    }

    fn collect(
        &mut self,
        importer: &mut impl ASTImporter,
        to_visit: &mut Vec<Name>,
        visited: &mut HashSet<Name>,
    ) {
        while let Some(name) = to_visit.pop() {
            //try to import the ast, if the importer isn't able to achieve this and returns None,
            //Ignore this ast analysis. It'll be up to the given importer implementation to handle the
            //errors caused by this import request failure
            if let Some((imported, name)) = import_ast(name, importer, visited) {
                self.collect_ast_symbols(imported, name, to_visit)
            }
        }
    }

    fn collect_ast_symbols(
        &mut self,
        imported: Imported,
        module_name: Name,
        to_visit: &mut Vec<Name>,
    ) {
        // Immediately transfer the ownership of the AST to the engine.
        let root_block = self.engine.take(imported.expr);

        let env = Environment::script(module_name);
        let mut state = ResolutionState::new(
            imported.content,
            self.engine.track(imported.content, root_block),
        );
        self.engine.attach(state.module, env);
        self.stack.push(state.module);

        self.tree_walk(&mut state, root_block, to_visit);
        self.stack.pop();
    }

    fn add_checked_import(
        &mut self,
        mod_id: SourceId,
        import: UnresolvedImport,
        import_expr: &'e ImportExpr,
        import_fqn: Name,
    ) {
        if let Some(shadowed) =
            self.imports
                .add_unresolved_import(mod_id, import, import_expr.segment())
        {
            let reef = self.externals.current;
            let diagnostic = Diagnostic::new(
                DiagnosticID::ShadowedImport,
                format!("{import_fqn} is imported twice."),
            )
            .with_observation(Observation::here(
                mod_id,
                reef,
                shadowed,
                "useless import here",
            ))
            .with_observation(Observation::context(
                mod_id,
                reef,
                import_expr.segment(),
                "This statement shadows previous import",
            ));
            self.diagnostics.push(diagnostic)
        }
    }

    /// Collects the symbol import and place it as an [UnresolvedImport] in the relations.
    fn collect_symbol_import(
        &mut self,
        import: &'e ImportExpr,
        mut relative_path: Vec<InclusionPathItem>,
        mod_id: SourceId,
        to_visit: &mut Vec<Name>,
    ) {
        let reef = self.externals.current;
        match import {
            ImportExpr::Symbol(s) => {
                relative_path.extend(s.path.iter().cloned());
                match SymbolLocation::compute(&relative_path) {
                    Ok(loc) => {
                        let alias = s.alias.as_ref().map(|s| s.to_string());

                        let name = loc.name.clone();
                        to_visit.push(name.clone());

                        let unresolved = UnresolvedImport::Symbol { alias, loc };
                        self.add_checked_import(mod_id, unresolved, import, name)
                    }
                    Err(segments) => self
                        .diagnostics
                        .push(make_invalid_path_diagnostic(mod_id, reef, segments)),
                }
            }
            ImportExpr::AllIn(items, _) => {
                relative_path.extend(items.iter().cloned());
                match SymbolLocation::compute(&relative_path) {
                    Ok(loc) => {
                        let name = loc.name.clone();
                        to_visit.push(name.clone());
                        let unresolved = UnresolvedImport::AllIn(loc);
                        self.add_checked_import(mod_id, unresolved, import, name)
                    }
                    Err(segments) => self
                        .diagnostics
                        .push(make_invalid_path_diagnostic(mod_id, reef, segments)),
                }
            }

            ImportExpr::Environment(_) => {
                let diagnostic = Diagnostic::new(
                    DiagnosticID::UnsupportedFeature,
                    "import of environment variables and commands are not yet supported.",
                )
                .with_observation((mod_id, reef, import.segment()).into());

                self.diagnostics.push(diagnostic);
            }
            ImportExpr::List(list) => {
                relative_path.extend(list.root.iter().cloned());

                match SymbolLocation::compute(&list.root) {
                    Ok(_) => {
                        for list_import in &list.imports {
                            self.collect_symbol_import(
                                list_import,
                                relative_path.clone(),
                                mod_id,
                                to_visit,
                            )
                        }
                    }
                    Err(segments) => self
                        .diagnostics
                        .push(make_invalid_path_diagnostic(mod_id, reef, segments)),
                }
            }
        }
    }

    fn tree_walk(&mut self, state: &mut ResolutionState, expr: &'e Expr, to_visit: &mut Vec<Name>) {
        match expr {
            Expr::Use(import) => {
                if !state.accept_imports {
                    let diagnostic = Diagnostic::new(
                        DiagnosticID::UseBetweenExprs,
                        "Unexpected use statement between expressions. Use statements must be at the top of the environment.",
                    ).with_observation((state.module, self.externals.current, import.segment()).into());
                    self.diagnostics.push(diagnostic);
                    return;
                }
                self.collect_symbol_import(&import.import, Vec::new(), state.module, to_visit);
                return;
            }
            Expr::Assign(assign) => {
                self.tree_walk(state, &assign.left, to_visit);
                self.tree_walk(state, &assign.value, to_visit);
            }
            Expr::Binary(binary) => {
                self.tree_walk(state, &binary.left, to_visit);
                self.tree_walk(state, &binary.right, to_visit);
            }
            Expr::Match(match_expr) => {
                self.tree_walk(state, &match_expr.operand, to_visit);
                for arm in &match_expr.arms {
                    for pattern in &arm.patterns {
                        match pattern {
                            MatchPattern::VarRef(reference) => {
                                if let VarName::User(name) = &reference.name {
                                    let symbol = self.identify_symbol(
                                        *self.stack.last().unwrap(),
                                        state.module,
                                        SymbolLocation::unspecified(Name::new(name)),
                                        reference.segment(),
                                        SymbolRegistry::Objects,
                                    );
                                    self.current_env().annotate(reference, symbol);
                                }
                            }
                            MatchPattern::Template(template) => {
                                for part in &template.parts {
                                    self.tree_walk(state, part, to_visit);
                                }
                            }
                            MatchPattern::Literal(_) | MatchPattern::Wildcard(_) => {}
                        }
                    }
                    if let Some(guard) = &arm.guard {
                        self.current_env().begin_scope();
                        self.tree_walk(state, guard, to_visit);
                        self.current_env().end_scope();
                    }
                    self.current_env().begin_scope();
                    if let Some(name) = &arm.val_name {
                        self.current_env()
                            .symbols
                            .declare_local(name.to_string(), SymbolInfo::Variable);
                    }
                    self.tree_walk(state, &arm.body, to_visit);
                    self.current_env().end_scope();
                }
            }
            Expr::Call(call) => {
                self.resolve_special_call(*self.stack.last().unwrap(), call);
                for arg in &call.arguments {
                    self.tree_walk(state, arg, to_visit);
                }
            }
            Expr::ProgrammaticCall(call) => {
                match SymbolLocation::compute(&call.path) {
                    Ok(loc) => {
                        let symbol = self.identify_symbol(
                            *self.stack.last().unwrap(),
                            state.module,
                            loc,
                            call.segment(),
                            SymbolRegistry::Objects,
                        );

                        self.current_env().annotate(call, symbol);
                    }
                    Err(segments) => self.diagnostics.push(make_invalid_path_diagnostic(
                        state.module,
                        self.externals.current,
                        segments,
                    )),
                }

                for ty in &call.type_parameters {
                    self.collect_type(state.module, ty);
                }

                for arg in &call.arguments {
                    self.tree_walk(state, arg, to_visit);
                }
            }
            Expr::MethodCall(call) => {
                self.tree_walk(state, &call.source, to_visit);
                for targ in &call.type_parameters {
                    self.collect_type(state.module, targ)
                }
                for arg in &call.arguments {
                    self.tree_walk(state, arg, to_visit);
                }
            }
            Expr::Pipeline(pipeline) => {
                for expr in &pipeline.commands {
                    self.tree_walk(state, expr, to_visit);
                }
            }
            Expr::Redirected(redirected) => {
                self.tree_walk(state, &redirected.expr, to_visit);
                for redir in &redirected.redirections {
                    self.tree_walk(state, &redir.operand, to_visit);
                }
            }
            Expr::Detached(detached) => {
                self.tree_walk(state, &detached.underlying, to_visit);
            }
            Expr::Path(ident) => match SymbolLocation::compute(&ident.path) {
                Ok(loc) => {
                    let symbol = self.identify_symbol(
                        *self.stack.last().unwrap(),
                        state.module,
                        loc,
                        ident.segment(),
                        SymbolRegistry::Objects,
                    );
                    self.current_env().annotate(ident, symbol);
                }
                Err(segments) => self.diagnostics.push(make_invalid_path_diagnostic(
                    state.module,
                    self.externals.current,
                    segments,
                )),
            },
            Expr::VarDeclaration(var) => {
                if let Some(initializer) = &var.initializer {
                    self.tree_walk(state, initializer, to_visit);
                }
                if let Some(ty) = &var.var.ty {
                    self.collect_type(*self.stack.last().unwrap(), ty)
                }
                let env = self.current_env();
                let symbol = env
                    .symbols
                    .declare_local(var.var.name.to_string(), SymbolInfo::Variable);
                env.annotate(var, SymbolRef::Local(symbol));
            }
            Expr::VarReference(var) => {
                if let VarName::User(name) = &var.name {
                    if is_magic_variable_name(name) {
                        let script_env = self
                            .engine
                            .get_environment_mut(*self.stack.first().unwrap())
                            .unwrap();
                        if script_env
                            .symbols
                            .find_reachable(
                                "",
                                SymbolRegistry::Magic(MagicSymbolKind::ProgramArguments),
                            )
                            .is_none()
                        {
                            script_env
                                .symbols
                                .declare_magic(MagicSymbolKind::ProgramArguments);
                        }
                    }

                    let symbol = self.identify_symbol(
                        *self.stack.last().unwrap(),
                        state.module,
                        SymbolLocation::unspecified(Name::new(name)),
                        var.segment(),
                        SymbolRegistry::Objects,
                    );
                    self.current_env().annotate(var, symbol);
                }
            }
            Expr::FieldAccess(access) => {
                self.tree_walk(state, &access.expr, to_visit);
            }
            Expr::Range(range) => match range {
                Iterable::Range(range) => {
                    self.tree_walk(state, &range.start, to_visit);
                    self.tree_walk(state, &range.end, to_visit);
                    if let Some(step) = &range.step {
                        self.tree_walk(state, step, to_visit);
                    }
                    let struct_name = if range.upper_inclusive {
                        "InclusiveRange"
                    } else {
                        "Range"
                    };
                    let name = Name::from(vec!["std".to_owned(), struct_name.to_owned()]);
                    let symbol = self.identify_symbol(
                        *self.stack.last().unwrap(),
                        state.module,
                        SymbolLocation {
                            name,
                            is_current_reef_explicit: false,
                        },
                        range.segment(),
                        SymbolRegistry::Objects,
                    );
                    self.current_env().annotate(range, symbol);
                }
                Iterable::Files(pattern) => {
                    self.tree_walk(state, &pattern.pattern, to_visit);
                }
            },
            Expr::Subscript(sub) => {
                self.tree_walk(state, &sub.target, to_visit);
                self.tree_walk(state, &sub.index, to_visit);
            }
            Expr::Tilde(tilde) => {
                let function_name = match &tilde.structure {
                    Tilde::HomeDir(Some(expr)) => {
                        self.tree_walk(state, expr, to_visit);
                        "home_dir"
                    }
                    Tilde::HomeDir(None) => "current_home_dir",
                    Tilde::WorkingDir => "working_dir",
                };
                let name = Name::from(vec!["std".to_owned(), function_name.to_owned()]);
                let symbol = self.identify_symbol(
                    *self.stack.last().unwrap(),
                    state.module,
                    SymbolLocation {
                        name,
                        is_current_reef_explicit: false,
                    },
                    tilde.segment(),
                    SymbolRegistry::Objects,
                );
                self.current_env().annotate(tilde, symbol);
            }
            Expr::Substitution(sub) => {
                self.current_env().begin_scope();
                for expr in &sub.underlying.expressions {
                    self.tree_walk(state, expr, to_visit);
                }
                self.current_env().end_scope();
            }
            Expr::TemplateString(template) => {
                for expr in &template.parts {
                    self.tree_walk(state, expr, to_visit);
                }
            }
            Expr::Casted(casted) => {
                self.collect_type(*self.stack.last().unwrap(), &casted.casted_type);
                self.tree_walk(state, &casted.expr, to_visit);
            }
            Expr::Test(test) => {
                self.tree_walk(state, &test.expression, to_visit);
            }
            Expr::Unary(unary) => {
                self.tree_walk(state, &unary.expr, to_visit);
            }
            Expr::Parenthesis(paren) => {
                self.tree_walk(state, &paren.expression, to_visit);
            }
            Expr::Subshell(subshell) => {
                self.current_env().begin_scope();
                for expr in &subshell.expressions {
                    self.tree_walk(state, expr, to_visit);
                }
                self.current_env().end_scope();
            }
            Expr::Block(block) => {
                self.current_env().begin_scope();
                for expr in &block.expressions {
                    self.tree_walk(state, expr, to_visit);
                }
                self.current_env().end_scope();
            }
            Expr::If(if_expr) => {
                self.current_env().begin_scope();
                self.tree_walk(state, &if_expr.condition, to_visit);
                self.current_env().end_scope();
                self.current_env().begin_scope();
                self.tree_walk(state, &if_expr.success_branch, to_visit);
                self.current_env().end_scope();
                if let Some(else_branch) = &if_expr.fail_branch {
                    self.current_env().begin_scope();
                    self.tree_walk(state, else_branch, to_visit);
                    self.current_env().end_scope();
                }
            }
            Expr::While(wh) => {
                self.current_env().begin_scope();
                self.tree_walk(state, &wh.condition, to_visit);
                self.current_env().end_scope();
                self.current_env().begin_scope();
                self.tree_walk(state, &wh.body, to_visit);
                self.current_env().end_scope();
            }
            Expr::Loop(lp) => {
                self.current_env().begin_scope();
                self.tree_walk(state, &lp.body, to_visit);
                self.current_env().end_scope();
            }
            Expr::For(fr) => {
                self.current_env().begin_scope();
                match fr.kind.as_ref() {
                    ForKind::Range(range) => {
                        let env = self.current_env();
                        let symbol = env
                            .symbols
                            .declare_local(range.receiver.to_string(), SymbolInfo::Variable);
                        env.annotate(range, SymbolRef::Local(symbol));
                        self.tree_walk(state, &range.iterable, to_visit);
                    }
                    ForKind::Conditional(cond) => {
                        self.tree_walk(state, &cond.initializer, to_visit);
                        self.tree_walk(state, &cond.condition, to_visit);
                        self.tree_walk(state, &cond.increment, to_visit);
                    }
                }
                self.tree_walk(state, &fr.body, to_visit);
                self.current_env().end_scope();
            }
            Expr::Return(ret) => {
                if let Some(expr) = &ret.expr {
                    self.tree_walk(state, expr, to_visit);
                }
            }
            Expr::FunctionDeclaration(func) => {
                let symbol = self
                    .current_env()
                    .symbols
                    .declare_local(func.name.value.to_string(), SymbolInfo::Function);
                self.current_env().annotate(func, SymbolRef::Local(symbol));

                let func_id = self.engine().track(state.content, expr);
                self.current_env().bind_source(func, func_id);
                let func_env = self
                    .current_env()
                    .fork(state.module, func.name.value.as_ref());

                self.stack.push(func_id);

                let func_env = self.engine().attach(func_id, func_env);

                for type_param in &func.type_parameters {
                    func_env
                        .symbols
                        .declare_local(type_param.name.to_string(), SymbolInfo::Type);
                    if !type_param.params.is_empty() {
                        unimplemented!("Parametrized type parameters are not yet supported");
                    }
                }

                for param in &func.parameters {
                    let param_name = match param {
                        FunctionParameter::Named(named) => {
                            if let Some(ty) = &named.ty {
                                self.collect_type(func_id, ty);
                            }
                            named.name.to_string()
                        }
                        FunctionParameter::Variadic(_, _) => "@".to_owned(),
                        FunctionParameter::Slf(_) => continue,
                    };
                    let func_env = self.engine().get_environment_mut(func_id).unwrap();

                    let symbol = func_env
                        .symbols
                        .declare_local(param_name, SymbolInfo::Variable);

                    // Only named parameters can be annotated for now
                    if let FunctionParameter::Named(named) = param {
                        func_env.annotate(named, SymbolRef::Local(symbol));
                    }
                }
                if let Some(ty) = &func.return_type {
                    self.collect_type(func_id, ty)
                }

                if let Some(body) = &func.body {
                    self.tree_walk(&mut state.fork(func_id), body, to_visit);
                }

                Self::resolve_captures(
                    &self.stack,
                    self.engine,
                    self.relations,
                    self.externals.current,
                    &mut self.diagnostics,
                );
                self.stack.pop();
            }
            Expr::LambdaDef(lambda) => {
                let func_id = self.engine().track(state.content, expr);

                let func_env = self
                    .current_env()
                    .fork(state.module, &format!("lambda@{}", func_id.0));

                self.stack.push(func_id);
                self.engine().attach(func_id, func_env);

                for param in &lambda.args {
                    let func_env = self.engine().get_environment_mut(func_id).unwrap();
                    let symbol = func_env
                        .symbols
                        .declare_local(param.name.value.as_str().to_owned(), SymbolInfo::Variable);
                    func_env.annotate(param, SymbolRef::Local(symbol));

                    if let Some(ty) = &param.ty {
                        self.collect_type(func_id, ty)
                    }
                }
                self.tree_walk(&mut state.fork(func_id), &lambda.body, to_visit);
                Self::resolve_captures(
                    &self.stack,
                    self.engine,
                    self.relations,
                    self.externals.current,
                    &mut self.diagnostics,
                );
                self.stack.pop();
            }
            Expr::StructDeclaration(decl) => {
                let struct_env_id = self.engine().track(state.content, expr);

                let struct_env = self
                    .current_env()
                    .fork(state.module, decl.name.value.as_str());

                let local_id = self
                    .current_env()
                    .symbols
                    .declare_local(decl.name.to_string(), SymbolInfo::Type);

                let struct_env = self.engine().attach(struct_env_id, struct_env);

                for tparam in &decl.parameters {
                    struct_env
                        .symbols
                        .declare_local(tparam.name.to_string(), SymbolInfo::Type);
                }

                for attribute in &decl.fields {
                    let struct_env = self.engine().get_environment_mut(struct_env_id).unwrap();
                    struct_env
                        .symbols
                        .declare_local(attribute.name.to_string(), SymbolInfo::Variable);
                    self.collect_type(struct_env_id, &attribute.tpe);
                }

                self.current_env().bind_source(decl, struct_env_id);
                self.current_env()
                    .annotate(decl, SymbolRef::Local(local_id));
            }
            Expr::Literal(_) | Expr::Continue(_) | Expr::Break(_) => {}
            Expr::Impl(_) => todo!(),
        }
        state.accept_imports = false;
    }

    fn resolve_captures(
        stack: &[SourceId],
        engine: &Engine,
        relations: &mut Relations,
        reef: ReefId,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        let stack: Vec<_> = stack
            .iter()
            .map(|id| (*id, engine.get_environment(*id).unwrap()))
            .collect();
        SymbolResolver::resolve_captures(&stack, relations, reef, diagnostics);
    }

    fn collect_type(&mut self, origin: SourceId, ty: &Type) {
        match ty {
            Type::Parametrized(p) => match SymbolLocation::compute(&p.path) {
                Err(segments) => self.diagnostics.push(make_invalid_path_diagnostic(
                    origin,
                    self.externals.current,
                    segments,
                )),
                Ok(loc) => {
                    for param in &p.params {
                        self.collect_type(origin, param)
                    }
                    let symref = self.identify_symbol(
                        origin,
                        origin,
                        loc,
                        p.segment(),
                        SymbolRegistry::Types,
                    );
                    let origin_env = self.engine().get_environment_mut(origin).unwrap();
                    origin_env.annotate(p, symref)
                }
            },
            Type::Callable(_) | Type::ByName(_) => {
                panic!("Callable and By Name types are not yet supported.")
            }
        }
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

    /// perform special operations if the bound call is a special call that may introduce new variables.
    fn resolve_special_call(&mut self, env_id: SourceId, call: &Call) -> bool {
        let Some(command) = self.extract_literal_argument(call, 0) else {
            return false;
        };
        match command {
            "read" => {
                if let Some(var) = self.extract_literal_argument(call, 1) {
                    let env = self.engine().get_environment_mut(env_id).unwrap();
                    let symbol = env
                        .symbols
                        .declare_local(var.to_owned(), SymbolInfo::Variable);
                    env.annotate(&call.arguments[1], SymbolRef::Local(symbol));
                }
                true
            }
            "cd" => {
                let name = Name::from(vec!["std".to_owned(), command.to_owned()]);
                let symbol = self.identify_symbol(
                    *self.stack.last().unwrap(),
                    env_id,
                    SymbolLocation {
                        name,
                        is_current_reef_explicit: false,
                    },
                    call.arguments[0].segment().clone(),
                    SymbolRegistry::Objects,
                );
                self.current_env().annotate(call, symbol);
                true
            }
            _ => false,
        }
    }

    /// Identifies a [SymbolRef] from given source.
    /// Will return [SymbolRef::Local] if the given name isn't qualified and was found in the current environment
    /// Else, if the symbol does not exists, [SymbolRef::External] is returned and a new relation is requested for resolution.
    fn identify_symbol(
        &mut self,
        source: SourceId,
        origin: SourceId,
        location: SymbolLocation,
        segment: SourceSegment,
        registry: SymbolRegistry,
    ) -> SymbolRef {
        let symbols = &mut self.engine.get_environment_mut(source).unwrap().symbols;

        macro_rules! track_global {
            () => {
                *symbols
                    .external(location)
                    .or_insert_with(|| self.relations.track_new_object(origin, registry))
            };
        }

        //if a reef is explicitly specified, then the reef and symbol's name must be resolved first
        if location.is_current_reef_explicit {
            return SymbolRef::External(track_global!());
        }

        let local_symbol = symbols.find_reachable(location.name.root(), registry);

        // If the requested registry is Object, and the symbol wasn't found, then try to search it in the types registry.
        let local_symbol = if registry == SymbolRegistry::Objects {
            local_symbol
                .or_else(|| symbols.find_reachable(location.name.root(), SymbolRegistry::Types))
        } else {
            local_symbol
        };

        match local_symbol {
            None => SymbolRef::External(track_global!()),
            Some(id) if location.name.is_qualified() => {
                let var = symbols.get(id).unwrap();
                self.diagnostics.push(diagnose_invalid_symbol(
                    var.ty,
                    origin,
                    self.externals.current,
                    &location.name,
                    &[segment],
                ));
                // instantly declare a dead resolution object
                // We could have returned None here to ignore the symbol but it's more appropriate to
                // bind the variable occurrence with a dead object to signify that its bound symbol is invalid.
                let id = track_global!();
                self.relations[id].state = RelationState::Dead;
                SymbolRef::External(id)
            }
            Some(id) => SymbolRef::Local(id),
        }
    }
}

fn import_ast(
    name: Name,
    importer: &mut impl ASTImporter,
    visited: &mut HashSet<Name>,
) -> Option<(Imported, Name)> {
    let mut parts = name.into_vec();
    while !parts.is_empty() {
        let name = Name::from(parts.clone());
        if !visited.insert(name.clone()) {
            return None;
        }
        match importer.import(&name) {
            ImportResult::Success(imported) => return Some((imported, name)),
            ImportResult::NotFound => {
                // Nothing has been found, but we might have a chance by
                // importing the parent module.
                parts.pop();
            }
            ImportResult::Failure => {
                // Something has been found, but cannot be fully imported,
                // so don't try to import anything else.
                return None;
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

fn make_invalid_path_diagnostic(
    source: SourceId,
    reef: ReefId,
    bad_segments: Vec<SourceSegment>,
) -> Diagnostic {
    Diagnostic::new(
        DiagnosticID::InvalidSymbolPath,
        "Symbol path contains invalid items",
    )
    .with_observations(
        bad_segments
            .into_iter()
            .map(|s| Observation::context(source, reef, s, "Invalid path item")),
    )
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use context::str_find::{find_in, find_in_nth};
    use parser::parse_trusted;

    use crate::importer::StaticImporter;
    use crate::relations::{LocalId, RelationId};

    use super::*;

    fn tree_walk<'e>(
        expr: &'e Expr,
        engine: &mut Engine<'e>,
        relations: &mut Relations,
    ) -> (Vec<Diagnostic>, Environment) {
        let env = Environment::script(Name::new("test"));
        let mut imports = Imports::default();
        let externals = Externals::default();
        let mut state = ResolutionState::new(ContentId(0), engine.track(ContentId(0), expr));
        let mut collector = SymbolCollector::new(engine, relations, &mut imports, &externals);
        collector.engine.attach(SourceId(0), env);
        collector.stack.push(SourceId(0));
        collector.tree_walk(&mut state, expr, &mut vec![]);
        let env = collector.engine.get_environment(SourceId(0)).unwrap();
        collector.stack.pop();
        (collector.diagnostics, env.clone())
    }

    #[test]
    fn use_between_expressions() {
        let content = "use a; $a; use c; $c";
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut importer = StaticImporter::new([(Name::new("test"), content)], parse_trusted);
        let res = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut vec![Name::new("test")],
            &mut HashSet::new(),
            &mut importer,
        );
        assert_eq!(
            res,
            vec![
                Diagnostic::new(DiagnosticID::UseBetweenExprs, "Unexpected use statement between expressions. Use statements must be at the top of the environment.")
                    .with_observation((
                        SourceId(0),
                        ReefId(1),
                        find_in(content, "use c"),
                    ).into()),
            ]
        )
    }

    #[test]
    fn bind_local_variables() {
        let expr = parse_trusted("var bar = 4; $bar");
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let diagnostics = tree_walk(&expr, &mut engine, &mut relations).0;
        assert_eq!(diagnostics, vec![]);
        assert_eq!(relations.iter().collect::<Vec<_>>(), vec![]);
    }

    #[test]
    fn test_symbol_clashes_with_module() {
        let math_source = "use math::{add, multiply, divide}; fun multiply(a: Int, b: Int) = a * b";
        let math_src = math_source;
        let empty_source = "";

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let externals = Externals::default();
        let mut importer = StaticImporter::new(
            [
                (Name::new("math"), math_src),
                (Name::new("math::multiply"), empty_source),
                (Name::new("math::add"), empty_source),
                (Name::new("math::divide"), empty_source),
            ],
            parse_trusted,
        );

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &externals,
            &mut vec![Name::new("math")],
            &mut HashSet::new(),
            &mut importer,
        );
        assert_eq!(diagnostics, vec![
            Diagnostic::new(DiagnosticID::SymbolConflictsWithModule, "Declared symbol 'multiply' in module math clashes with module math::multiply")
                .with_observation(Observation::here(SourceId(0), ReefId(1), find_in(math_source, "fun multiply(a: Int, b: Int) = a * b"), "This symbol has the same fully-qualified name as module math::multiply"))
                .with_help("You should refactor this symbol with a name that does not conflicts with following modules: math::{divide, multiply, add}")
        ]);
    }

    #[test]
    fn shadowed_imports() {
        let source = "use A; use B; use A; use B";
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut importer = StaticImporter::new([(Name::new("test"), source)], parse_trusted);

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut vec![Name::new("test")],
            &mut HashSet::new(),
            &mut importer,
        );

        assert_eq!(
            diagnostics,
            vec![
                Diagnostic::new(DiagnosticID::ShadowedImport, "A is imported twice.")
                    .with_observation(Observation::here(
                        SourceId(0),
                        ReefId(1),
                        find_in(source, "A"),
                        "useless import here"
                    ))
                    .with_observation(Observation::context(
                        SourceId(0),
                        ReefId(1),
                        find_in_nth(source, "A", 1),
                        "This statement shadows previous import"
                    )),
                Diagnostic::new(DiagnosticID::ShadowedImport, "B is imported twice.")
                    .with_observation(Observation::here(
                        SourceId(0),
                        ReefId(1),
                        find_in(source, "B"),
                        "useless import here"
                    ))
                    .with_observation(Observation::context(
                        SourceId(0),
                        ReefId(1),
                        find_in_nth(source, "B", 1),
                        "This statement shadows previous import"
                    )),
            ]
        )
    }

    #[test]
    fn bind_function_param() {
        let source = "fun id(a) = return $a";
        let expr = parse_trusted(source);
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let (diagnostics, env) = tree_walk(&expr, &mut engine, &mut relations);
        assert_eq!(diagnostics, vec![]);
        assert_eq!(relations.iter().collect::<Vec<_>>(), vec![]);
        assert_eq!(
            env.get_raw_symbol(source.segment()),
            Some(SymbolRef::Local(LocalId(0)))
        );
        assert_eq!(env.get_raw_symbol(find_in(source, "a")), None);
        assert_eq!(env.get_raw_symbol(find_in(source, "$a")), None);
        let func_env = engine.get_environment(SourceId(1)).unwrap();
        assert_eq!(
            func_env.get_raw_symbol(find_in(source, "a")),
            Some(SymbolRef::Local(LocalId(0)))
        );
        assert_eq!(
            func_env.get_raw_symbol(find_in(source, "$a")),
            Some(SymbolRef::Local(LocalId(0)))
        );
    }

    #[test]
    fn bind_primitive() {
        let source = "read foo";
        let expr = parse_trusted(source);
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let (diagnostics, env) = tree_walk(&expr, &mut engine, &mut relations);
        assert_eq!(diagnostics, vec![]);
        assert_eq!(relations.iter().collect::<Vec<_>>(), vec![]);
        assert_eq!(env.get_raw_symbol(find_in(source, "read")), None);
        assert_eq!(
            env.get_raw_symbol(find_in(source, "foo")),
            Some(SymbolRef::Local(LocalId(0)))
        );
    }

    #[test]
    fn find_references() {
        let source = "$bar; baz($foo, $bar)";
        let expr = parse_trusted(source);

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let (diagnostics, _) = tree_walk(&expr, &mut engine, &mut relations);
        assert_eq!(diagnostics, vec![]);
        assert_eq!(
            relations
                .find_references(&engine, RelationId(0))
                .map(|mut references| {
                    references.sort_by_key(|range| range.start);
                    references
                }),
            Some(vec![
                find_in(source, "$bar"),
                find_in_nth(source, "$bar", 1)
            ])
        );
        assert_eq!(
            relations.find_references(&engine, RelationId(1)),
            Some(vec![find_in(source, "baz($foo, $bar)")])
        );
        assert_eq!(
            relations.find_references(&engine, RelationId(2)),
            Some(vec![find_in(source, "$foo")])
        );
    }
}
