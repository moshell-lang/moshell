use crate::engine::Engine;
use crate::environment::variables::TypeInfo;
use crate::environment::Environment;
use crate::importer::Importer;
use crate::name::Name;
use crate::resolver::{Resolver, SourceObjectId, UnresolvedImport};
use crate::steps::GatherError;
use ast::call::Call;
use ast::control_flow::ForKind;
use ast::function::FunctionParameter;
use ast::group::Block;
use ast::r#match::MatchPattern;
use ast::r#use::Import as AstImport;
use ast::range::Iterable;
use ast::value::LiteralValue;
use ast::Expr;
use context::source::{Source, SourceSegmentHolder};
use parser::parse;
use std::collections::HashSet;

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

fn import_source<'a>(
    name: Name,
    importer: &mut impl Importer<'a>,
) -> Result<(Source<'a>, Name), String> {
    let mut parts = name.parts().to_vec();
    while !parts.is_empty() {
        let name = Name::from(parts.clone());
        match importer.import(&name) {
            Ok(source) => return Ok((source, name)),
            Err(_) => {
                parts.pop();
            }
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
        let (source, name) = import_source(name, importer).map_err(GatherError::Other)?;
        let report = parse(source);
        if report.is_err() {
            return Err(GatherError::Parse(report.errors));
        }

        // Immediately transfer the ownership of the AST to the engine.
        let root_block = {
            let expressions = report.unwrap();
            let root_block = Expr::Block(Block {
                expressions,
                segment: source.segment(),
            });
            engine.take(root_block)
        };

        let mut env = Environment::named(name);
        let mut state = ResolutionState::new(engine.track(root_block));
        tree_walk(
            engine,
            resolver,
            &mut env,
            &mut state,
            &mut visitable,
            root_block,
        )
        .map_err(GatherError::Other)?;
        engine.attach(state.module, env)
    }
    Ok(())
}

/// Collects the symbol import and place it as an [UnresolvedImport] in the resolver.
fn collect_symbol_import(
    import: &AstImport,
    relative_path: Vec<String>,
    resolver: &mut Resolver,
    visitable: &mut Vec<Name>,
    mod_id: SourceObjectId,
) -> Result<(), String> {
    match import {
        AstImport::Symbol(s) => {
            let mut symbol_name = relative_path;
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
            let mut symbol_name = relative_path;
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

                collect_symbol_import(list_import, relative, resolver, visitable, mod_id)?
            }
            Ok(())
        }
    }
}

fn tree_walk<'a>(
    engine: &mut Engine<'a>,
    resolver: &mut Resolver,
    env: &mut Environment,
    state: &mut ResolutionState,
    visitable: &mut Vec<Name>,
    expr: &'a Expr,
) -> Result<(), String> {
    match expr {
        Expr::Use(import) => {
            if !state.accept_imports {
                return Err("Unexpected use statement between expressions. use statements can only be declared on top of environment".to_owned());
            }
            collect_symbol_import(
                &import.import,
                Vec::new(),
                resolver,
                visitable,
                state.module,
            )?;
            return Ok(());
        }
        Expr::Assign(assign) => {
            tree_walk(engine, resolver, env, state, visitable, &assign.value)?;
        }
        Expr::Binary(binary) => {
            tree_walk(engine, resolver, env, state, visitable, &binary.left)?;
            tree_walk(engine, resolver, env, state, visitable, &binary.right)?;
        }
        Expr::Match(match_expr) => {
            tree_walk(engine, resolver, env, state, visitable, &match_expr.operand)?;
            for arm in &match_expr.arms {
                for pattern in &arm.patterns {
                    match pattern {
                        MatchPattern::VarRef(reference) => {
                            let symbol =
                                env.variables
                                    .identify(state.module, resolver, reference.name);
                            env.annotate(reference, symbol);
                        }
                        MatchPattern::Template(template) => {
                            for part in &template.parts {
                                tree_walk(engine, resolver, env, state, visitable, part)?;
                            }
                        }
                        MatchPattern::Literal(_) | MatchPattern::Wildcard(_) => {}
                    }
                }
                if let Some(guard) = &arm.guard {
                    env.begin_scope();
                    tree_walk(engine, resolver, env, state, visitable, guard)?;
                    env.end_scope();
                }
                env.begin_scope();
                if let Some(name) = arm.val_name {
                    env.variables
                        .declare_local(name.to_owned(), TypeInfo::Variable);
                }
                tree_walk(engine, resolver, env, state, visitable, &arm.body)?;
                env.end_scope();
            }
        }
        Expr::Call(call) => {
            resolve_primitive(env, call);
            for arg in &call.arguments {
                tree_walk(engine, resolver, env, state, visitable, arg)?;
            }
        }
        Expr::ProgrammaticCall(call) => {
            let symbol = env.variables.identify(state.module, resolver, call.name);
            env.annotate(call, symbol);
            for arg in &call.arguments {
                tree_walk(engine, resolver, env, state, visitable, arg)?;
            }
        }
        Expr::MethodCall(call) => {
            tree_walk(engine, resolver, env, state, visitable, &call.source)?;
            for arg in &call.arguments {
                tree_walk(engine, resolver, env, state, visitable, arg)?;
            }
        }
        Expr::Pipeline(pipeline) => {
            for expr in &pipeline.commands {
                tree_walk(engine, resolver, env, state, visitable, expr)?;
            }
        }
        Expr::Redirected(redirected) => {
            tree_walk(engine, resolver, env, state, visitable, &redirected.expr)?;
            for redir in &redirected.redirections {
                tree_walk(engine, resolver, env, state, visitable, &redir.operand)?;
            }
        }
        Expr::Detached(detached) => {
            tree_walk(
                engine,
                resolver,
                env,
                state,
                visitable,
                &detached.underlying,
            )?;
        }
        Expr::VarDeclaration(var) => {
            if let Some(initializer) = &var.initializer {
                tree_walk(engine, resolver, env, state, visitable, initializer)?;
            }
            let symbol = env
                .variables
                .declare_local(var.var.name.to_owned(), TypeInfo::Variable);
            env.annotate(var, symbol);
        }
        Expr::VarReference(var) => {
            let symbol = env.variables.identify(state.module, resolver, var.name);
            env.annotate(var, symbol);
        }
        Expr::Range(range) => match range {
            Iterable::Range(range) => {
                tree_walk(engine, resolver, env, state, visitable, &range.start)?;
                tree_walk(engine, resolver, env, state, visitable, &range.end)?;
            }
            Iterable::Files(_) => {}
        },
        Expr::Substitution(sub) => {
            env.begin_scope();
            for expr in &sub.underlying.expressions {
                tree_walk(engine, resolver, env, state, visitable, expr)?;
            }
            env.end_scope();
        }
        Expr::TemplateString(template) => {
            for expr in &template.parts {
                tree_walk(engine, resolver, env, state, visitable, expr)?;
            }
        }
        Expr::Casted(casted) => {
            tree_walk(engine, resolver, env, state, visitable, &casted.expr)?;
        }
        Expr::Test(test) => {
            tree_walk(engine, resolver, env, state, visitable, &test.expression)?;
        }
        Expr::Not(not) => {
            tree_walk(engine, resolver, env, state, visitable, &not.underlying)?;
        }
        Expr::Parenthesis(paren) => {
            tree_walk(engine, resolver, env, state, visitable, &paren.expression)?;
        }
        Expr::Subshell(subshell) => {
            env.begin_scope();
            for expr in &subshell.expressions {
                tree_walk(engine, resolver, env, state, visitable, expr)?;
            }
            env.end_scope();
        }
        Expr::Block(block) => {
            env.begin_scope();
            for expr in &block.expressions {
                tree_walk(engine, resolver, env, state, visitable, expr)?;
            }
            env.end_scope();
        }
        Expr::If(if_expr) => {
            env.begin_scope();
            tree_walk(engine, resolver, env, state, visitable, &if_expr.condition)?;
            env.end_scope();
            env.begin_scope();
            tree_walk(
                engine,
                resolver,
                env,
                state,
                visitable,
                &if_expr.success_branch,
            )?;
            env.end_scope();
            if let Some(else_branch) = &if_expr.fail_branch {
                env.begin_scope();
                tree_walk(engine, resolver, env, state, visitable, else_branch)?;
                env.end_scope();
            }
        }
        Expr::While(wh) => {
            env.begin_scope();
            tree_walk(engine, resolver, env, state, visitable, &wh.condition)?;
            env.end_scope();
            env.begin_scope();
            tree_walk(engine, resolver, env, state, visitable, &wh.body)?;
            env.end_scope();
        }
        Expr::Loop(lp) => {
            env.begin_scope();
            tree_walk(engine, resolver, env, state, visitable, &lp.body)?;
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
                    tree_walk(engine, resolver, env, state, visitable, &range.iterable)?;
                }
                ForKind::Conditional(cond) => {
                    tree_walk(engine, resolver, env, state, visitable, &cond.initializer)?;
                    tree_walk(engine, resolver, env, state, visitable, &cond.condition)?;
                    tree_walk(engine, resolver, env, state, visitable, &cond.increment)?;
                }
            }
            tree_walk(engine, resolver, env, state, visitable, &fr.body)?;
            env.end_scope();
        }
        Expr::Return(ret) => {
            if let Some(expr) = &ret.expr {
                tree_walk(engine, resolver, env, state, visitable, expr)?;
            }
        }
        Expr::FunctionDeclaration(func) => {
            let symbol = env
                .variables
                .declare_local(func.name.to_owned(), TypeInfo::Function);
            env.annotate(func, symbol);
            let func_id = engine.track(expr);
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
            tree_walk(
                engine,
                resolver,
                &mut func_env,
                &mut ResolutionState::new(func_id),
                visitable,
                &func.body,
            )?;
            engine.attach(func_id, func_env);
        }
        Expr::LambdaDef(lambda) => {
            let func_id = engine.track(expr);
            let mut func_env = env.fork(func_id, &format!("lambda@{}", func_id.0));
            for param in &lambda.args {
                let symbol = func_env
                    .variables
                    .declare_local(param.name.to_owned(), TypeInfo::Variable);
                func_env.annotate(param, symbol);
            }
            tree_walk(
                engine,
                resolver,
                &mut func_env,
                &mut ResolutionState::new(func_id),
                visitable,
                &lambda.body,
            )?;
            engine.attach(func_id, func_env);
        }
        Expr::Literal(_) | Expr::Continue(_) | Expr::Break(_) => {}
    }
    state.accept_imports = false;
    Ok(())
}

fn extract_literal_argument<'a>(call: &'a Call, nth: usize) -> Option<&'a str> {
    match call.arguments.get(nth)? {
        Expr::Literal(lit) => match &lit.parsed {
            LiteralValue::String(str) => Some(str),
            _ => None,
        },
        _ => None,
    }
}

fn resolve_primitive(env: &mut Environment, call: &Call) -> Option<()> {
    let command = extract_literal_argument(call, 0)?;
    match command {
        "read" => {
            let var = extract_literal_argument(call, 1)?;
            let symbol = env
                .variables
                .declare_local(var.to_owned(), TypeInfo::Variable);
            env.annotate(&call.arguments[1], symbol);
            Some(())
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::importer::StaticImporter;
    use crate::resolver::{GlobalObjectId, Symbol};
    use ast::call::ProgrammaticCall;
    use ast::function::{FunctionDeclaration, Return};
    use ast::group::Block;
    use ast::value::Literal;
    use ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};

    #[test]
    fn use_between_expressions() {
        let mut engine = Engine::default();
        let mut resolver = Resolver::default();
        let mut importer =
            StaticImporter::new([(Name::new("test"), Source::unknown("use a; $a; use c; $c"))]);
        let res = collect_symbols(&mut engine, &mut resolver, Name::new("test"), &mut importer);
        assert_eq!(res,
                   Err(GatherError::Other("Unexpected use statement between expressions. use statements can only be declared on top of environment".to_string())))
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
        let mut resolver = Resolver::default();
        let mut env = Environment::named(Name::new("test"));
        let mut state = ResolutionState::new(engine.track(&expr));

        tree_walk(
            &mut engine,
            &mut resolver,
            &mut env,
            &mut state,
            &mut Vec::new(),
            &expr,
        )
        .expect("tree walk");
        assert_eq!(resolver.objects.len(), 0);
    }

    #[test]
    fn bind_function_param() {
        let expr = Expr::FunctionDeclaration(FunctionDeclaration {
            name: "id",
            type_parameters: vec![],
            parameters: vec![FunctionParameter::Named(TypedVariable {
                name: "n",
                ty: None,
                segment: 3..4,
            })],
            return_type: None,
            body: Box::new(Expr::Return(Return {
                expr: Some(Box::new(Expr::VarReference(VarReference {
                    name: "n",
                    segment: 13..15,
                }))),
                segment: 8..17,
            })),
            segment: 0..17,
        });
        let mut engine = Engine::default();
        let mut resolver = Resolver::default();
        let mut env = Environment::named(Name::new("test"));
        let mut state = ResolutionState::new(engine.track(&expr));
        tree_walk(
            &mut engine,
            &mut resolver,
            &mut env,
            &mut state,
            &mut Vec::new(),
            &expr,
        )
        .expect("tree walk");
        assert_eq!(resolver.objects.len(), 0);
        assert_eq!(env.get_raw_symbol(0..17), Some(Symbol::Local(0)));
        assert_eq!(env.get_raw_symbol(3..4), None);
        assert_eq!(env.get_raw_symbol(13..15), None);
        let func_env = engine.find_environment(SourceObjectId(1)).unwrap();
        assert_eq!(func_env.get_raw_symbol(3..4), Some(Symbol::Local(0)));
        assert_eq!(func_env.get_raw_symbol(13..15), Some(Symbol::Local(0)));
    }

    #[test]
    fn bind_primitive() {
        let expr = Expr::Call(Call {
            path: vec![],
            arguments: vec![
                Expr::Literal(Literal {
                    parsed: "read".into(),
                    segment: 0..5,
                }),
                Expr::Literal(Literal {
                    parsed: "foo".into(),
                    segment: 6..9,
                }),
            ],
            type_parameters: vec![],
        });
        let mut engine = Engine::default();
        let mut resolver = Resolver::default();
        let mut env = Environment::named(Name::new("test"));
        let mut state = ResolutionState::new(engine.track(&expr));
        tree_walk(
            &mut engine,
            &mut resolver,
            &mut env,
            &mut state,
            &mut Vec::new(),
            &expr,
        )
        .expect("tree walk");
        assert_eq!(resolver.objects.len(), 0);
        assert_eq!(env.get_raw_symbol(0..5), None);
        assert_eq!(env.get_raw_symbol(6..9), Some(Symbol::Local(0)));
    }

    #[test]
    fn find_references() {
        let expr = Expr::Block(Block {
            expressions: vec![
                Expr::VarReference(VarReference {
                    name: "bar",
                    segment: 0..4,
                }),
                Expr::ProgrammaticCall(ProgrammaticCall {
                    path: vec![],
                    name: "baz",
                    arguments: vec![
                        Expr::VarReference(VarReference {
                            name: "foo",
                            segment: 9..13,
                        }),
                        Expr::VarReference(VarReference {
                            name: "bar",
                            segment: 15..19,
                        }),
                    ],
                    type_parameters: vec![],
                    segment: 5..20,
                }),
            ],
            segment: 0..20,
        });
        let mut engine = Engine::default();
        let mut resolver = Resolver::default();
        let mut env = Environment::named(Name::new("test"));
        let mut state = ResolutionState::new(engine.track(&expr));
        tree_walk(
            &mut engine,
            &mut resolver,
            &mut env,
            &mut state,
            &mut Vec::new(),
            &expr,
        )
        .expect("tree walk");
        engine.attach(state.module, env);
        assert_eq!(
            resolver
                .find_references(&engine, GlobalObjectId(0))
                .map(|mut references| {
                    references.sort_by_key(|range| range.start);
                    references
                }),
            Some(vec![0..4, 15..19])
        );
        assert_eq!(
            resolver.find_references(&engine, GlobalObjectId(1)),
            Some(vec![5..20])
        );
        assert_eq!(
            resolver.find_references(&engine, GlobalObjectId(2)),
            Some(vec![9..13])
        );
    }
}
