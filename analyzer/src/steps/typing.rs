use crate::dependency::topological_sort;
use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::environment::Environment;
use crate::relations::{Relations, SourceObjectId};
use crate::types::ctx::TypeContext;
use crate::types::engine::{Chunk, TypedEngine};
use crate::types::exploration::Exploration;
use crate::types::function::{infer_return, type_call, type_parameter, Return};
use crate::types::hir::{ExprKind, TypedExpr};
use crate::types::ty::Type;
use crate::types::{Typing, ERROR, FLOAT, INT, NOTHING, STRING};
use ast::value::LiteralValue;
use ast::Expr;
use context::source::SourceSegmentHolder;

pub fn apply_types(
    engine: &Engine,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedEngine {
    let environments = topological_sort(&relations.build_dependencies(engine));
    let mut exploration = Exploration {
        engine: TypedEngine::new(engine.len()),
        typing: Typing::lang(),
        ctx: TypeContext::lang(),
        returns: Vec::new(),
    };
    for env_id in environments {
        let entry = apply_types_to_source(
            &mut exploration,
            diagnostics,
            engine,
            relations,
            TypingState::new(env_id),
        );
        exploration.engine.insert(env_id, entry);
    }
    exploration.engine
}

/// A state holder, used to informs the type checker about what should be
/// checked.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct TypingState {
    source: SourceObjectId,
    local_type: bool,
}

impl TypingState {
    fn new(source: SourceObjectId) -> Self {
        Self {
            source,
            local_type: false,
        }
    }

    fn with_local_type(self) -> Self {
        Self {
            local_type: true,
            ..self
        }
    }
}

fn apply_types_to_source(
    exploration: &mut Exploration,
    diagnostics: &mut Vec<Diagnostic>,
    engine: &Engine,
    relations: &Relations,
    state: TypingState,
) -> Chunk {
    let source_id = state.source;
    let expr = engine.get_expression(source_id).unwrap();
    let env = engine.get_environment(source_id).unwrap();
    exploration.prepare(source_id);
    match expr {
        Expr::FunctionDeclaration(func) => {
            for param in &func.parameters {
                let param = type_parameter(&exploration.ctx, param);
                exploration.ctx.push_local_type(param.ty);
            }
            let typed_expr = ascribe_types(
                exploration,
                relations,
                diagnostics,
                env,
                &func.body,
                state.with_local_type(),
            );
            let return_type = infer_return(func, &typed_expr, diagnostics, exploration);
            Chunk::function(
                typed_expr,
                func.parameters
                    .iter()
                    .map(|param| type_parameter(&exploration.ctx, param))
                    .collect(),
                return_type,
            )
        }
        expr => Chunk::script(ascribe_types(
            exploration,
            relations,
            diagnostics,
            env,
            expr,
            state,
        )),
    }
}

/// Ascribes types to the given expression.
///
/// In case of an error, the expression is still returned, but the type is set to [`ERROR`].
fn ascribe_types(
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    expr: &Expr,
    state: TypingState,
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
                        exploration,
                        relations,
                        diagnostics,
                        env,
                        expr,
                        state.with_local_type(),
                    ))
                })
                .expect("Variables without initializers are not supported yet");
            exploration.ctx.push_local_type(initializer.ty);
            if let Some(type_annotation) = &decl.var.ty {
                let expected_type = exploration.ctx.resolve(type_annotation).unwrap_or(ERROR);
                if expected_type == ERROR {
                    diagnostics.push(
                        Diagnostic::new(
                            DiagnosticID::UnknownType,
                            state.source,
                            "Unknown type annotation",
                        )
                        .with_observation(Observation::with_help(
                            type_annotation.segment(),
                            "Not found in scope",
                        )),
                    );
                } else if initializer.ty.is_ok()
                    && exploration
                        .typing
                        .unify(expected_type, initializer.ty)
                        .is_err()
                {
                    diagnostics.push(
                        Diagnostic::new(DiagnosticID::TypeMismatch, state.source, "Type mismatch")
                            .with_observation(Observation::with_help(
                                type_annotation.segment(),
                                format!(
                                    "Expected `{}`",
                                    exploration.get_type(expected_type).unwrap()
                                ),
                            ))
                            .with_observation(Observation::with_help(
                                initializer.segment(),
                                format!(
                                    "Found `{}`",
                                    exploration.get_type(initializer.ty).unwrap()
                                ),
                            )),
                    );
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
            let type_id = exploration.ctx.get(relations, symbol).unwrap();
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
                .filter(|expr| !matches!(expr, Expr::Use(_)))
                .map(|expr| ascribe_types(exploration, relations, diagnostics, env, expr, state))
                .collect::<Vec<_>>();
            let ty = expressions.last().map(|expr| expr.ty).unwrap_or(NOTHING);
            TypedExpr {
                kind: ExprKind::Block(expressions),
                ty,
                segment: block.segment.clone(),
            }
        }
        Expr::Return(ret) => {
            let expr = ret.expr.as_ref().map(|expr| {
                Box::new(ascribe_types(
                    exploration,
                    relations,
                    diagnostics,
                    env,
                    expr,
                    state,
                ))
            });
            exploration.returns.push(Return {
                ty: expr.as_ref().map(|expr| expr.ty).unwrap_or(NOTHING),
                segment: ret.segment.clone(),
            });
            TypedExpr {
                kind: ExprKind::Return(expr),
                ty: NOTHING,
                segment: ret.segment.clone(),
            }
        }
        Expr::Parenthesis(paren) => ascribe_types(
            exploration,
            relations,
            diagnostics,
            env,
            &paren.expression,
            state,
        ),
        Expr::FunctionDeclaration(fun) => {
            let declaration = env.get_raw_env(fun.segment.clone()).unwrap();
            let type_id = exploration.typing.add_type(Type::Function(declaration));
            exploration.ctx.push_local_type(type_id);

            // Forward declare the function
            let parameters = fun
                .parameters
                .iter()
                .map(|param| type_parameter(&exploration.ctx, param))
                .collect::<Vec<_>>();
            let return_type = fun
                .return_type
                .as_ref()
                .map(|ty| exploration.ctx.resolve(ty).unwrap_or(ERROR))
                .unwrap_or(NOTHING);
            exploration.engine.insert_if_absent(
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
                ascribe_types(exploration, relations, diagnostics, env, &bin.left, state);
            let right_expr =
                ascribe_types(exploration, relations, diagnostics, env, &bin.right, state);
            let ty = exploration
                .typing
                .unify(left_expr.ty, right_expr.ty)
                .unwrap_or(ERROR);
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
                exploration,
                relations,
                diagnostics,
                env,
                &block.condition,
                state,
            );
            let then = ascribe_types(
                exploration,
                relations,
                diagnostics,
                env,
                &block.success_branch,
                state,
            );
            let otherwise = block.fail_branch.as_ref().map(|expr| {
                Box::new(ascribe_types(
                    exploration,
                    relations,
                    diagnostics,
                    env,
                    expr,
                    state,
                ))
            });
            let ty = if state.local_type {
                match exploration.typing.unify(
                    then.ty,
                    otherwise.as_ref().map(|expr| expr.ty).unwrap_or(NOTHING),
                ) {
                    Ok(ty) => ty,
                    Err(_) => {
                        let mut diagnostic = Diagnostic::new(
                            DiagnosticID::TypeMismatch,
                            state.source,
                            "`if` and `else` have incompatible types",
                        )
                        .with_observation(Observation::with_help(
                            block.success_branch.segment(),
                            format!("Found `{}`", exploration.get_type(then.ty).unwrap()),
                        ));
                        if let Some(otherwise) = &otherwise {
                            diagnostic = diagnostic.with_observation(Observation::with_help(
                                otherwise.segment(),
                                format!("Found `{}`", exploration.get_type(otherwise.ty).unwrap()),
                            ));
                        }
                        diagnostics.push(diagnostic);
                        ERROR
                    }
                }
            } else {
                NOTHING
            };
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
                .map(|expr| ascribe_types(exploration, relations, diagnostics, env, expr, state))
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
                .map(|expr| ascribe_types(exploration, relations, diagnostics, env, expr, state))
                .collect::<Vec<_>>();
            let symbol = env.get_raw_symbol(call.segment.clone()).unwrap();
            let return_type = type_call(
                call,
                &arguments,
                symbol,
                diagnostics,
                exploration,
                relations,
            );
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
        let typed = apply_types(&engine, &relations, &mut diagnostics);
        let expr = typed.get(SourceObjectId(0)).unwrap();
        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }
        Ok(typing.get_type(expr.expression.ty).unwrap().clone())
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
        let content = "val a: Int = 1.6";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "Type mismatch",
            )
            .with_observation(Observation::with_help(
                find_in(content, "Int"),
                "Expected `Int`",
            ))
            .with_observation(Observation::with_help(
                find_in(content, "1.6"),
                "Found `Float`",
            ))])
        );
    }

    #[test]
    fn unknown_type_annotation() {
        let content = "val a: ABC = 1.6";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::UnknownType,
                SourceObjectId(0),
                "Unknown type annotation",
            )
            .with_observation(Observation::with_help(
                find_in(content, "ABC"),
                "Not found in scope",
            ))])
        );
    }

    #[test]
    fn condition_same_type() {
        let res = extract_type(Source::unknown("if true; 1; else 2"));
        assert_eq!(res, Ok(Type::Nothing));
    }

    #[test]
    fn condition_different_type() {
        let res = extract_type(Source::unknown("if false; 4.7; else {}"));
        assert_eq!(res, Ok(Type::Nothing));
    }

    #[test]
    fn condition_different_type_local_return() {
        let content = "var n: Int = {if false; 4.7; else {}}";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "`if` and `else` have incompatible types",
            )
            .with_observation(Observation::with_help(
                find_in(content, "4.7"),
                "Found `Float`",
            ))
            .with_observation(Observation::with_help(
                find_in(content, "{}"),
                "Found `Nothing`",
            ))])
        );
    }

    #[test]
    fn function_return_type() {
        let res = extract_type(Source::unknown("fun one() -> Int = 1\none()"));
        assert_eq!(res, Ok(Type::Int));
    }

    #[test]
    fn wrong_arguments() {
        let content = "fun square(n: Int) -> Int = $(( $n * $n ))\nsquare(9, 9)";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "This function takes 1 argument but 2 were supplied",
            )
            .with_observation(Observation::with_help(
                find_in(content, "square(9, 9)"),
                "Function is called here"
            ))])
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
        let content = "val test = 1;test()";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "Cannot invoke non function type",
            )
            .with_observation(Observation::with_help(
                find_in(content, "test()"),
                "Call expression requires function, found `Int`"
            ))])
        );
    }

    #[test]
    fn type_function_parameters() {
        let content = "fun test(a: String) = { var b: Int = $a }";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(1),
                "Type mismatch",
            )
            .with_observation(Observation::with_help(
                find_in(content, "Int"),
                "Expected `Int`"
            ))
            .with_observation(Observation::with_help(
                find_in(content, "$a"),
                "Found `String`"
            ))])
        );
    }

    #[test]
    fn a_calling_b() {
        let res = extract_type(Source::unknown(
            "fun a() -> Int = b()\nfun b() -> Int = 1\na()",
        ));
        assert_eq!(res, Ok(Type::Int));
    }

    #[test]
    fn bidirectional_usage() {
        let res = extract_type(Source::unknown(
            "val PI = 3.14\nfun circle(r: Float) -> Float = $(( $PI * $r * $r ))\ncircle(1)",
        ));
        assert_eq!(res, Ok(Type::Float));
    }

    #[test]
    fn incorrect_return_type() {
        let content = "fun zero() -> String = 0";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(1),
                "Type mismatch",
            )
            .with_observation(Observation::with_help(find_in(content, "0"), "Found `Int`"))
            .with_observation(Observation::with_help(
                find_in(content, "String"),
                "Expected `String` because of return type"
            ))])
        );
    }

    #[test]
    fn explicit_valid_return() {
        let content = "fun some() -> Int = return 20";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(Type::Nothing));
    }

    #[test]
    fn explicit_valid_return_mixed() {
        let content = "fun some() -> Int = {\nif true; return 5; 9\n}";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(Type::Nothing));
    }

    #[test]
    fn explicit_invalid_return() {
        let content = "fun some() -> String = {if true; return {}; 9}";
        let res = extract_type(Source::unknown(content));
        let return_observation = Observation::with_help(
            find_in(content, "String"),
            "Expected `String` because of return type",
        );
        assert_eq!(
            res,
            Err(vec![
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    SourceObjectId(1),
                    "Type mismatch",
                )
                .with_observation(Observation::with_help(
                    find_in(content, "return {}"),
                    "Found `Nothing`"
                ))
                .with_observation(return_observation.clone()),
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    SourceObjectId(1),
                    "Type mismatch",
                )
                .with_observation(Observation::with_help(find_in(content, "9"), "Found `Int`"))
                .with_observation(return_observation)
            ])
        );
    }

    #[test]
    fn infer_valid_return_type() {
        let content = "fun test(n: Float) = if false; 0.0; else $n; test(156.0)";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::CannotInfer,
                SourceObjectId(1),
                "Return type inference is not supported yet",
            )
            .with_observation(Observation::with_help(
                find_in(content, "fun test(n: Float) = "),
                "No return type is specified"
            ))
            .with_tip("Add -> Float to the function declaration")])
        );
    }

    #[test]
    fn no_infer_block_return_type() {
        let content = "fun test(n: Float) = {if false; return 0; $n}; test(156.0)";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::CannotInfer,
                SourceObjectId(1),
                "Return type is not inferred for block functions",
            )
            .with_observation(Observation::with_help(
                find_in(content, "return 0"),
                "Returning `Int`"
            ))
            .with_observation(Observation::with_help(
                find_in(content, "$n"),
                "Returning `Float`"
            ))
            .with_tip("Try adding an explicit return type to the function")])
        );
    }

    #[test]
    fn no_infer_complex_return_type() {
        let content = "fun test() = if false; return 5; else {}; test()";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::CannotInfer,
                SourceObjectId(1),
                "Failed to infer return type",
            )
            .with_observation(Observation::with_help(
                find_in(content, "fun test() = if false; return 5; else {}"),
                "This function returns multiple types"
            ))
            .with_tip("Try adding an explicit return type to the function")])
        );
    }
}
