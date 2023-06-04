use crate::dependency::topological_sort;
use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::environment::Environment;
use crate::relations::{Relations, SourceObjectId};
use crate::types::ctx::TypeContext;
use crate::types::engine::{Chunk, TypedEngine};
use crate::types::hir::{ExprKind, TypedExpr};
use crate::types::ty::{Parameter, Type};
use crate::types::{Typing, ERROR, FLOAT, INT, NOTHING, STRING};
use ast::function::FunctionParameter;
use ast::value::LiteralValue;
use ast::Expr;
use context::source::SourceSegmentHolder;

pub fn apply_types(
    engine: &Engine,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedEngine {
    let mut typing = Typing::lang();
    let mut ctx = TypeContext::lang();
    let environments = topological_sort(&relations.build_dependencies(engine));
    let mut typed = TypedEngine::new(engine.len());
    for env_id in environments {
        let entry = apply_types_to_source(
            &mut typed,
            engine,
            relations,
            diagnostics,
            &mut typing,
            &mut ctx,
            env_id,
        );
        typed.insert(env_id, entry);
    }
    typed
}

fn apply_types_to_source(
    typed: &mut TypedEngine,
    engine: &Engine,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    typing: &mut Typing,
    ctx: &mut TypeContext,
    source_id: SourceObjectId,
) -> Chunk {
    let expr = engine.get_expression(source_id).unwrap();
    let env = engine.get_environment(source_id).unwrap();
    ctx.prepare(source_id);
    match expr {
        Expr::FunctionDeclaration(func) => {
            for param in &func.parameters {
                let param = type_parameter(ctx, param);
                ctx.push_local_type(param.ty);
            }
            let typed_expr = ascribe_types(
                typed,
                relations,
                diagnostics,
                typing,
                ctx,
                env,
                &func.body,
            );
            Chunk::function(
                typed_expr,
                func.parameters
                    .iter()
                    .map(|param| type_parameter(ctx, param))
                    .collect(),
                func.return_type
                    .as_ref()
                    .map(|ty| ctx.resolve(ty).unwrap_or(ERROR))
                    .unwrap_or(NOTHING),
            )
        }
        expr => Chunk::script(ascribe_types(
            typed,
            relations,
            diagnostics,
            typing,
            ctx,
            env,
            expr,
        )),
    }
}

/// Ascribes types to the given expression.
///
/// In case of an error, the expression is still returned, but the type is set to [`ERROR`].
fn ascribe_types(
    engine: &mut TypedEngine,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    typing: &mut Typing,
    ctx: &mut TypeContext,
    env: &Environment,
    expr: &Expr,
) -> TypedExpr {
    match expr {
        Expr::Literal(lit) => {
            let ty = match lit.parsed {
                LiteralValue::Int(_) => INT,
                LiteralValue::Float(_) => FLOAT,
                LiteralValue::String(_) => STRING,
            };
            TypedExpr {
                kind: ExprKind::Literal(lit.parsed.clone()),
                ty,
                segment: lit.segment.clone(),
            }
        }
        Expr::VarDeclaration(decl) => {
            let initializer = decl
                .initializer
                .as_ref()
                .map(|expr| {
                    Box::new(ascribe_types(
                        engine,
                        relations,
                        diagnostics,
                        typing,
                        ctx,
                        env,
                        expr,
                    ))
                })
                .expect("Variables without initializers are not supported yet");
            ctx.push_local_type(initializer.ty);
            if let Some(type_annotation) = &decl.var.ty {
                let type_annotation = ctx.resolve(type_annotation).unwrap_or(ERROR);
                if type_annotation == ERROR {
                    diagnostics.push(Diagnostic::new(
                        DiagnosticID::UnknownType,
                        ctx.source,
                        "Unknown type annotation",
                    ));
                } else if typing.unify(type_annotation, initializer.ty).is_err() {
                    diagnostics.push(Diagnostic::new(
                        DiagnosticID::TypeMismatch,
                        ctx.source,
                        "Type mismatch",
                    ));
                }
            }
            TypedExpr {
                kind: ExprKind::Declare {
                    name: decl.var.name.to_owned(),
                    value: Some(initializer),
                },
                ty: NOTHING,
                segment: decl.segment.clone(),
            }
        }
        Expr::VarReference(var) => {
            let symbol = env.get_raw_symbol(var.segment.clone()).unwrap();
            let type_id = ctx.get(relations, symbol).unwrap();
            TypedExpr {
                kind: ExprKind::Reference {
                    name: var.name.to_owned(),
                },
                ty: type_id,
                segment: var.segment.clone(),
            }
        }
        Expr::Block(block) => {
            let expressions = block
                .expressions
                .iter()
                .map(|expr| ascribe_types(engine, relations, diagnostics, typing, ctx, env, expr))
                .collect::<Vec<_>>();
            let ty = expressions.last().map(|expr| expr.ty).unwrap_or(NOTHING);
            TypedExpr {
                kind: ExprKind::Block(expressions),
                ty,
                segment: block.segment.clone(),
            }
        }
        Expr::Parenthesis(paren) => ascribe_types(
            engine,
            relations,
            diagnostics,
            typing,
            ctx,
            env,
            &paren.expression,
        ),
        Expr::FunctionDeclaration(fun) => {
            let declaration = env.get_raw_env(fun.segment.clone()).unwrap();
            let type_id = typing.add_type(Type::Function(declaration));
            ctx.push_local_type(type_id);

            // Forward declare the function
            let parameters = fun
                .parameters
                .iter()
                .map(|param| type_parameter(ctx, param))
                .collect::<Vec<_>>();
            let return_type = fun
                .return_type
                .as_ref()
                .map(|ty| ctx.resolve(ty).unwrap_or(ERROR))
                .unwrap_or(NOTHING);
            engine.insert(
                declaration,
                Chunk::function(
                    TypedExpr {
                        kind: ExprKind::Noop,
                        ty: type_id,
                        segment: fun.segment.clone(),
                    },
                    parameters,
                    return_type,
                ),
            );
            TypedExpr {
                kind: ExprKind::Declare {
                    name: fun.name.to_owned(),
                    value: None,
                },
                ty: NOTHING,
                segment: fun.segment.clone(),
            }
        }
        Expr::Binary(bin) => {
            let left_expr =
                ascribe_types(engine, relations, diagnostics, typing, ctx, env, &bin.left);
            let right_expr =
                ascribe_types(engine, relations, diagnostics, typing, ctx, env, &bin.right);
            let ty = typing.unify(left_expr.ty, right_expr.ty).unwrap_or(ERROR);
            TypedExpr {
                kind: ExprKind::Binary {
                    lhs: Box::new(left_expr),
                    op: bin.op,
                    rhs: Box::new(right_expr),
                },
                ty,
                segment: bin.segment(),
            }
        }
        Expr::If(block) => {
            let condition = ascribe_types(
                engine,
                relations,
                diagnostics,
                typing,
                ctx,
                env,
                &block.condition,
            );
            let then = ascribe_types(
                engine,
                relations,
                diagnostics,
                typing,
                ctx,
                env,
                &block.success_branch,
            );
            let otherwise = block.fail_branch.as_ref().map(|expr| {
                Box::new(ascribe_types(
                    engine,
                    relations,
                    diagnostics,
                    typing,
                    ctx,
                    env,
                    expr,
                ))
            });
            let ty = typing
                .unify(
                    then.ty,
                    otherwise.as_ref().map(|expr| expr.ty).unwrap_or(NOTHING),
                )
                .unwrap_or(ERROR);
            TypedExpr {
                kind: ExprKind::Conditional {
                    condition: Box::new(condition),
                    then: Box::new(then),
                    otherwise,
                },
                ty,
                segment: block.segment.clone(),
            }
        }
        Expr::Call(call) => {
            let args = call
                .arguments
                .iter()
                .map(|expr| ascribe_types(engine, relations, diagnostics, typing, ctx, env, expr))
                .collect::<Vec<_>>();
            TypedExpr {
                kind: ExprKind::ProcessCall(args),
                ty: NOTHING,
                segment: call.segment(),
            }
        }
        Expr::ProgrammaticCall(call) => {
            let arguments = call
                .arguments
                .iter()
                .map(|expr| ascribe_types(engine, relations, diagnostics, typing, ctx, env, expr))
                .collect::<Vec<_>>();
            let symbol = env.get_raw_symbol(call.segment.clone()).unwrap();
            let type_id = ctx.get(relations, symbol).unwrap();
            let return_type = match typing.get_type(type_id).unwrap() {
                Type::Function(declaration) => {
                    let entry = engine.get(declaration).unwrap();
                    let parameters = &entry.parameters;
                    let return_type = entry.return_type;
                    if parameters.len() != arguments.len() {
                        diagnostics.push(Diagnostic::new(
                            DiagnosticID::TypeMismatch,
                            ctx.source,
                            "Wrong number of arguments",
                        ));
                        ERROR
                    } else {
                        for (param, arg) in parameters.iter().zip(arguments.iter()) {
                            if typing.unify(param.ty, arg.ty).is_err() {
                                diagnostics.push(
                                    Diagnostic::new(
                                        DiagnosticID::TypeMismatch,
                                        ctx.source,
                                        "Type mismatch",
                                    )
                                    .with_observation(Observation::with_help(
                                        arg.segment.clone(),
                                        format!(
                                            "Expected `{}`, found `{}`",
                                            typing.get_type(param.ty).unwrap(),
                                            typing.get_type(arg.ty).unwrap()
                                        ),
                                    ))
                                    .with_observation(
                                        Observation::with_help(
                                            param.segment.clone(),
                                            "Parameter is declared here",
                                        ),
                                    ),
                                );
                            }
                        }
                        return_type
                    }
                }
                _ => {
                    diagnostics.push(Diagnostic::new(
                        DiagnosticID::TypeMismatch,
                        ctx.source,
                        "Cannot invoke non function type",
                    ));
                    ERROR
                }
            };
            TypedExpr {
                kind: ExprKind::FunctionCall {
                    name: call.name.to_owned(),
                    arguments,
                },
                ty: return_type,
                segment: call.segment.clone(),
            }
        }
        _ => todo!("{expr:?}"),
    }
}

fn type_parameter(ctx: &TypeContext, param: &FunctionParameter) -> Parameter {
    match param {
        FunctionParameter::Named(named) => {
            let type_id = named
                .ty
                .as_ref()
                .map(|ty| ctx.resolve(ty).unwrap_or(ERROR))
                .unwrap_or(STRING);
            Parameter {
                segment: named.segment.clone(),
                ty: type_id,
            }
        }
        FunctionParameter::Variadic(_) => todo!("Arrays are not supported yet"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::importer::StaticImporter;
    use crate::name::Name;
    use crate::steps::collect::SymbolCollector;
    use crate::steps::resolve::SymbolResolver;
    use crate::types::ty::Type;
    use context::source::Source;
    use context::str_find::find_in;
    use parser::parse_trusted;

    pub(crate) fn extract_type(source: Source) -> Result<Type, Vec<Diagnostic>> {
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let typing = Typing::lang();
        let name = Name::new(source.name);
        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            name.clone(),
            &mut StaticImporter::new([(name, source)], parse_trusted),
        );
        assert_eq!(diagnostics, vec![]);
        let mut diagnostics = SymbolResolver::resolve_symbols(&engine, &mut relations);
        assert_eq!(diagnostics, vec![]);
        let typed = apply_types(&mut engine, &mut relations, &mut diagnostics);
        let expr = typed.get(SourceObjectId(0)).unwrap();
        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }
        Ok(typing.get_type(expr.expression.ty).unwrap())
    }

    #[test]
    fn single_literal() {
        let res = extract_type(Source::unknown("1"));
        assert_eq!(res, Ok(Type::Int));
    }

    #[test]
    fn correct_type_annotation() {
        let res = extract_type(Source::unknown("val a: Int = 1"));
        assert_eq!(res, Ok(Type::Nothing));
    }

    #[test]
    fn coerce_type_annotation() {
        let res = extract_type(Source::unknown("val a: Float = 4"));
        assert_eq!(res, Ok(Type::Nothing));
    }

    #[test]
    fn no_coerce_type_annotation() {
        let res = extract_type(Source::unknown("val a: Int = 1.6"));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "Type mismatch",
            )])
        );
    }

    #[test]
    fn unknown_type_annotation() {
        let res = extract_type(Source::unknown("val a: ABC = 1.6"));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::UnknownType,
                SourceObjectId(0),
                "Unknown type annotation",
            )])
        );
    }

    #[test]
    fn condition_same_type() {
        let res = extract_type(Source::unknown("if true; 1; else 2"));
        assert_eq!(res, Ok(Type::Int));
    }

    #[test]
    fn function_return_type() {
        let res = extract_type(Source::unknown("fun one() -> Int = 1\none()"));
        assert_eq!(res, Ok(Type::Int));
    }

    #[test]
    fn wrong_arguments() {
        let res = extract_type(Source::unknown(
            "fun square(n: Int) = $(( $n * $n ))\nsquare(9, 9)",
        ));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "Wrong number of arguments",
            )])
        );
    }

    #[test]
    fn wrong_arguments_type() {
        let content = "fun dup(str: String) -> String = $str\ndup(4)";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "Type mismatch",
            )
            .with_observation(Observation::with_help(
                find_in(content, "4"),
                "Expected `String`, found `Int`"
            ))
            .with_observation(Observation::with_help(
                find_in(content, "str: String"),
                "Parameter is declared here"
            ))]),
        );
    }

    #[test]
    fn cannot_invoke_non_function() {
        let res = extract_type(Source::unknown("val test = 1;test()"));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "Cannot invoke non function type",
            )])
        );
    }

    #[test]
    fn type_function_parameters() {
        let res = extract_type(Source::unknown("fun test(a: String) = { var b: Int = $a }"));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(1),
                "Type mismatch",
            )])
        );
    }

    #[test]
    fn a_calling_b() {
        let res = extract_type(Source::unknown("fun a() -> Int = b()\nfun b() = 1\na()"));
        assert_eq!(res, Ok(Type::Int));
    }

    #[test]
    fn bidirectional_usage() {
        let res = extract_type(Source::unknown(
            "val PI = 3.14\nfun circle(r: Float) -> Float = $(( $PI * $r * $r ))\ncircle(1)",
        ));
        assert_eq!(res, Ok(Type::Float));
    }
}
