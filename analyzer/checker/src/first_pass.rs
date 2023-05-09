use crate::engine::Engine;
use crate::import::Importer;
use analyzer_system::environment::Environment;
use analyzer_system::name::Name;
use analyzer_system::resolver::{Resolver, SourceObjectId};
use analyzer_system::variables::TypeInfo;
use ast::control_flow::ForKind;
use ast::function::FunctionParameter;
use ast::group::Block;
use ast::r#match::MatchPattern;
use ast::r#use::Import;
use ast::range::Iterable;
use ast::Expr;
use context::source::SourceSegmentHolder;
use parser::err::ParseError;
use parser::parse;
use std::collections::HashSet;
use std::io;

#[derive(Debug, Clone, Copy)]
struct ResolutionState {
    module: SourceObjectId,
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

/// Explores the entry point and all its recursive dependencies.
///
/// This collects all the symbols that are used, locally or not yet resolved if they are global.
pub fn first_pass<'a>(
    engine: &mut Engine<'a>,
    resolver: &mut Resolver,
    entry_point: Name,
    importer: &mut impl Importer<'a>,
) -> Result<(), GatherError> {
    // Start by importing the entry point.
    resolver.visitable.push(entry_point);
    // Prevent re-importing the same names.
    let mut visited: HashSet<Name> = HashSet::new();

    while let Some(name) = resolver.visitable.pop() {
        if !visited.insert(name.clone()) {
            continue;
        }
        // Start by parsing the source read from the importer.
        let source = importer.import(&name)?;
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
        tree_walk(engine, resolver, &mut env, state, root_block);
        engine.attach(state.module, env)
    }
    Ok(())
}

fn tree_walk<'a>(
    engine: &mut Engine<'a>,
    resolver: &mut Resolver,
    env: &mut Environment,
    state: ResolutionState,
    expr: &'a Expr,
) {
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
        Expr::Assign(assign) => {
            tree_walk(engine, resolver, env, state, &assign.value);
        }
        Expr::Binary(binary) => {
            tree_walk(engine, resolver, env, state, &binary.left);
            tree_walk(engine, resolver, env, state, &binary.right);
        }
        Expr::Match(match_expr) => {
            tree_walk(engine, resolver, env, state, &match_expr.operand);
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
                                tree_walk(engine, resolver, env, state, part);
                            }
                        }
                        MatchPattern::Literal(_) | MatchPattern::Wildcard(_) => {}
                    }
                }
                if let Some(guard) = &arm.guard {
                    env.begin_scope();
                    tree_walk(engine, resolver, env, state, guard);
                    env.end_scope();
                }
                env.begin_scope();
                if let Some(name) = arm.val_name {
                    env.variables
                        .declare_local(name.to_owned(), TypeInfo::Variable);
                }
                tree_walk(engine, resolver, env, state, &arm.body);
                env.end_scope();
            }
        }
        Expr::Call(call) => {
            for arg in &call.arguments {
                tree_walk(engine, resolver, env, state, arg);
            }
        }
        Expr::ProgrammaticCall(call) => {
            let symbol = env.variables.identify(state.module, resolver, call.name);
            env.annotate(call, symbol);
            for arg in &call.arguments {
                tree_walk(engine, resolver, env, state, arg);
            }
        }
        Expr::MethodCall(call) => {
            tree_walk(engine, resolver, env, state, &call.source);
            for arg in &call.arguments {
                tree_walk(engine, resolver, env, state, arg);
            }
        }
        Expr::Pipeline(pipeline) => {
            for expr in &pipeline.commands {
                tree_walk(engine, resolver, env, state, expr);
            }
        }
        Expr::Redirected(redirected) => {
            tree_walk(engine, resolver, env, state, &redirected.expr);
            for redir in &redirected.redirections {
                tree_walk(engine, resolver, env, state, &redir.operand);
            }
        }
        Expr::Detached(detached) => {
            tree_walk(engine, resolver, env, state, &detached.underlying);
        }
        Expr::VarDeclaration(var) => {
            let symbol = env
                .variables
                .declare_local(var.var.name.to_owned(), TypeInfo::Variable);
            env.annotate(var, symbol);
            if let Some(initializer) = &var.initializer {
                tree_walk(engine, resolver, env, state, initializer);
            }
        }
        Expr::VarReference(var) => {
            let symbol = env.variables.identify(state.module, resolver, var.name);
            env.annotate(var, symbol);
        }
        Expr::Range(range) => match range {
            Iterable::Range(range) => {
                tree_walk(engine, resolver, env, state, &range.start);
                tree_walk(engine, resolver, env, state, &range.end);
            }
            Iterable::Files(_) => {}
        },
        Expr::Substitution(sub) => {
            env.begin_scope();
            for expr in &sub.underlying.expressions {
                tree_walk(engine, resolver, env, state, expr);
            }
            env.end_scope();
        }
        Expr::TemplateString(template) => {
            for expr in &template.parts {
                tree_walk(engine, resolver, env, state, expr);
            }
        }
        Expr::Casted(casted) => {
            tree_walk(engine, resolver, env, state, &casted.expr);
        }
        Expr::Test(test) => {
            tree_walk(engine, resolver, env, state, &test.expression);
        }
        Expr::Not(not) => {
            tree_walk(engine, resolver, env, state, &not.underlying);
        }
        Expr::Parenthesis(paren) => {
            tree_walk(engine, resolver, env, state, &paren.expression);
        }
        Expr::Subshell(subshell) => {
            env.begin_scope();
            for expr in &subshell.expressions {
                tree_walk(engine, resolver, env, state, expr);
            }
            env.end_scope();
        }
        Expr::Block(block) => {
            env.begin_scope();
            for expr in &block.expressions {
                tree_walk(engine, resolver, env, state, expr);
            }
            env.end_scope();
        }
        Expr::If(if_expr) => {
            env.begin_scope();
            tree_walk(engine, resolver, env, state, &if_expr.condition);
            env.end_scope();
            env.begin_scope();
            tree_walk(engine, resolver, env, state, &if_expr.success_branch);
            env.end_scope();
            if let Some(else_branch) = &if_expr.fail_branch {
                env.begin_scope();
                tree_walk(engine, resolver, env, state, else_branch);
                env.end_scope();
            }
        }
        Expr::While(wh) => {
            env.begin_scope();
            tree_walk(engine, resolver, env, state, &wh.condition);
            env.end_scope();
            env.begin_scope();
            tree_walk(engine, resolver, env, state, &wh.body);
            env.end_scope();
        }
        Expr::Loop(lp) => {
            env.begin_scope();
            tree_walk(engine, resolver, env, state, &lp.body);
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
                    tree_walk(engine, resolver, env, state, &range.iterable);
                }
                ForKind::Conditional(cond) => {
                    tree_walk(engine, resolver, env, state, &cond.initializer);
                    tree_walk(engine, resolver, env, state, &cond.condition);
                    tree_walk(engine, resolver, env, state, &cond.increment);
                }
            }
            tree_walk(engine, resolver, env, state, &fr.body);
            env.end_scope();
        }
        Expr::Return(ret) => {
            if let Some(expr) = &ret.expr {
                tree_walk(engine, resolver, env, state, expr);
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
            tree_walk(engine, resolver, &mut func_env, state, &func.body);
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
            tree_walk(engine, resolver, &mut func_env, state, &lambda.body);
            engine.attach(func_id, func_env);
        }
        Expr::Literal(_) | Expr::Continue(_) | Expr::Break(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use analyzer_system::resolver::Symbol;
    use ast::function::{FunctionDeclaration, Return};
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
                        segment: 0..3,
                    },
                    initializer: None,
                    segment: 0..3,
                }),
                Expr::VarReference(VarReference {
                    name: "bar",
                    segment: 4..7,
                }),
            ],
            segment: 0..7,
        });
        let mut engine = Engine::default();
        let mut resolver = Resolver::default();
        let mut env = Environment::named(Name::new("test"));
        let state = ResolutionState {
            module: engine.track(&expr),
        };
        tree_walk(&mut engine, &mut resolver, &mut env, state, &expr);
        assert_eq!(env.get_raw_symbol(0..3), Some(Symbol::Local(0)));
        assert_eq!(env.get_raw_symbol(4..7), Some(Symbol::Local(0)));
        assert_eq!(engine.origins.len(), 1);
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
        let state = ResolutionState {
            module: engine.track(&expr),
        };
        tree_walk(&mut engine, &mut resolver, &mut env, state, &expr);
        assert_eq!(engine.origins.len(), 2);
        assert_eq!(resolver.objects.len(), 0);
        assert_eq!(env.get_raw_symbol(0..17), Some(Symbol::Local(0)));
        assert_eq!(env.get_raw_symbol(3..4), None);
        assert_eq!(env.get_raw_symbol(13..15), None);
        let func_env = engine.origins[1].1.as_ref().unwrap();
        assert_eq!(func_env.get_raw_symbol(3..4), Some(Symbol::Local(0)));
        assert_eq!(func_env.get_raw_symbol(13..15), Some(Symbol::Local(0)));
    }
}
