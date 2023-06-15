use crate::dependency::topological_sort;
use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::environment::Environment;
use crate::relations::{Relations, SourceObjectId};
use crate::steps::typing::exploration::{diagnose_unknown_type, Exploration};
use crate::steps::typing::function::{
    find_operand_implementation, infer_return, type_call, type_method, type_parameter, Return,
};
use crate::steps::typing::lower::convert_into_string;
use crate::types::ctx::{TypeContext, TypedVariable};
use crate::types::engine::{Chunk, TypedEngine};
use crate::types::hir::{ExprKind, TypedExpr};
use crate::types::operator::name_operator_method;
use crate::types::ty::{Definition, Type};
use crate::types::{Typing, BOOL, ERROR, EXIT_CODE, FLOAT, INT, NOTHING, STRING};
use ast::operation::BinaryOperator;
use ast::value::LiteralValue;
use ast::variable::VarKind;
use ast::Expr;
use context::source::SourceSegmentHolder;

mod exploration;
mod function;
mod lower;

pub fn apply_types(
    engine: &Engine,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedEngine {
    let environments = topological_sort(&relations.as_dependencies(engine));
    let mut exploration = Exploration {
        engine: TypedEngine::with_lang(engine.len()),
        typing: Typing::with_lang(),
        ctx: TypeContext::with_lang(),
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
pub(self) struct TypingState {
    source: SourceObjectId,
    local_type: bool,
}

impl TypingState {
    /// Creates a new initial state, for a script.
    fn new(source: SourceObjectId) -> Self {
        Self {
            source,
            local_type: false,
        }
    }

    /// Returns a new state that should track local returns.
    fn with_local_type(self) -> Self {
        Self {
            local_type: true,
            ..self
        }
    }

    /// Returns a new state that indicates to not track local returns.
    fn without_local_type(self) -> Self {
        Self {
            local_type: false,
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
    exploration.prepare();
    match expr {
        Expr::FunctionDeclaration(func) => {
            for param in &func.parameters {
                let param = type_parameter(&exploration.ctx, param);
                exploration.ctx.push_local_type(state.source, param.ty);
            }
            let typed_expr = ascribe_types(
                exploration,
                relations,
                diagnostics,
                env,
                &func.body,
                state.with_local_type(),
            );
            let return_type = infer_return(func, &typed_expr, diagnostics, exploration, state);
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
        Expr::TemplateString(tpl) => {
            if tpl.parts.is_empty() {
                return TypedExpr {
                    kind: ExprKind::Literal(LiteralValue::String("".to_owned())),
                    ty: STRING,
                    segment: tpl.segment(),
                };
            }
            let mut it = tpl.parts.iter();
            let acc = ascribe_types(
                exploration,
                relations,
                diagnostics,
                env,
                it.next().unwrap(),
                state.without_local_type(),
            );
            let acc = convert_into_string(acc, exploration, diagnostics, state);
            let plus_method = exploration
                .engine
                .get_method_exact(
                    STRING,
                    name_operator_method(BinaryOperator::Plus),
                    &[STRING],
                    STRING,
                )
                .expect("string type should have a concatenation method")
                .definition;
            it.fold(acc, |acc, part| {
                let current = ascribe_types(
                    exploration,
                    relations,
                    diagnostics,
                    env,
                    part,
                    state.without_local_type(),
                );
                let segment = acc.segment.start..current.segment.end;
                TypedExpr {
                    kind: ExprKind::MethodCall {
                        callee: Box::new(convert_into_string(acc, exploration, diagnostics, state)),
                        arguments: vec![current],
                        definition: plus_method,
                    },
                    ty: STRING,
                    segment,
                }
            })
        }
        Expr::Assign(assign) => {
            let rhs = ascribe_types(
                exploration,
                relations,
                diagnostics,
                env,
                &assign.value,
                state.with_local_type(),
            );
            let symbol = env.get_raw_symbol(assign.segment()).unwrap();
            let var_obj = exploration
                .ctx
                .get(relations, state.source, symbol)
                .unwrap();
            let var_ty = var_obj.type_id;
            let rhs_type = rhs.ty;
            let rhs = match exploration.unify(var_ty, rhs, diagnostics, state) {
                Ok(rhs) => {
                    if !var_obj.can_reassign {
                        diagnostics.push(
                            Diagnostic::new(
                                DiagnosticID::CannotReassign,
                                state.source,
                                format!(
                                    "Cannot assign twice to immutable variable `{}`",
                                    assign.name
                                ),
                            )
                            .with_observation(Observation::with_help(
                                assign.segment(),
                                "Assignment happens here",
                            )),
                        );
                    }
                    rhs
                }
                Err(_) => {
                    diagnostics.push(
                        Diagnostic::new(
                            DiagnosticID::TypeMismatch,
                            state.source,
                            format!(
                                "Cannot assign a value of type `{}` to something of type `{}`",
                                exploration.get_type(rhs_type).unwrap(),
                                exploration.get_type(var_ty).unwrap()
                            ),
                        )
                        .with_observation(Observation::with_help(
                            assign.segment(),
                            "Assignment happens here",
                        )),
                    );
                    TypedExpr {
                        kind: ExprKind::Literal(LiteralValue::String("".to_owned())),
                        ty: STRING,
                        segment: assign.segment(),
                    }
                }
            };
            TypedExpr {
                kind: ExprKind::Assign {
                    identifier: symbol,
                    rhs: Box::new(rhs),
                },
                ty: NOTHING,
                segment: assign.segment(),
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
            let id = exploration.ctx.push_local(
                state.source,
                if decl.kind == VarKind::Val {
                    TypedVariable::immutable(initializer.ty)
                } else {
                    TypedVariable::assignable(initializer.ty)
                },
            );
            if let Some(type_annotation) = &decl.var.ty {
                let expected_type = exploration.ctx.resolve(type_annotation).unwrap_or(ERROR);
                if expected_type == ERROR {
                    diagnostics.push(diagnose_unknown_type(
                        state.source,
                        type_annotation.segment(),
                    ));
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
                    identifier: id,
                    value: Some(initializer),
                },
                ty: NOTHING,
                segment: decl.segment.clone(),
            }
        }
        Expr::VarReference(var) => {
            let symbol = env.get_raw_symbol(var.segment.clone()).unwrap();
            let type_id = exploration
                .ctx
                .get(relations, state.source, symbol)
                .unwrap()
                .type_id;
            TypedExpr {
                kind: ExprKind::Reference(symbol),
                ty: type_id,
                segment: var.segment.clone(),
            }
        }
        Expr::Block(block) => {
            let mut expressions = Vec::with_capacity(block.expressions.len());
            if let Some((last, exprs)) = block.expressions.split_last() {
                for expr in exprs {
                    expressions.push(ascribe_types(
                        exploration,
                        relations,
                        diagnostics,
                        env,
                        expr,
                        state.without_local_type(),
                    ));
                }
                expressions.push(ascribe_types(
                    exploration,
                    relations,
                    diagnostics,
                    env,
                    last,
                    state,
                ));
            }
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
            let type_id = exploration
                .typing
                .add_type(Type::Function(Definition::User(declaration)));
            let local_id = exploration.ctx.push_local_type(state.source, type_id);

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
                    identifier: local_id,
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
            let name = name_operator_method(bin.op);
            let method = exploration
                .engine
                .get_methods(left_expr.ty, name)
                .and_then(|methods| find_operand_implementation(methods, &right_expr));
            let ty = match method {
                Some(method) => method.return_type,
                _ => {
                    diagnostics.push(
                        Diagnostic::new(
                            DiagnosticID::UnknownMethod,
                            state.source,
                            "Undefined operator",
                        )
                        .with_observation(Observation::with_help(
                            bin.segment(),
                            format!(
                                "No operator `{}` between type `{}` and `{}`",
                                name,
                                exploration.get_type(left_expr.ty).unwrap(),
                                exploration.get_type(right_expr.ty).unwrap()
                            ),
                        )),
                    );
                    ERROR
                }
            };
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
        Expr::Casted(casted) => {
            let expr = ascribe_types(
                exploration,
                relations,
                diagnostics,
                env,
                &casted.expr,
                state,
            );
            let ty = exploration
                .ctx
                .resolve(&casted.casted_type)
                .unwrap_or(ERROR);
            if ty.is_err() {
                diagnostics.push(diagnose_unknown_type(
                    state.source,
                    casted.casted_type.segment(),
                ))
            } else if expr.ty.is_ok() && exploration.typing.unify(ty, expr.ty).is_err() {
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::IncompatibleCast,
                        state.source,
                        format!(
                            "Casting `{}` as `{}` is invalid",
                            exploration.get_type(expr.ty).unwrap(),
                            exploration.get_type(ty).unwrap()
                        ),
                    )
                    .with_observation(Observation::with_help(
                        casted.segment(),
                        "Incompatible cast",
                    )),
                );
            }
            TypedExpr {
                kind: ExprKind::Convert {
                    inner: Box::new(expr),
                    into: ty,
                },
                ty,
                segment: casted.segment(),
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
            let condition = match exploration.unify(BOOL, condition, diagnostics, state) {
                Ok(condition) => condition,
                Err(condition) => {
                    diagnostics.push(
                        Diagnostic::new(
                            DiagnosticID::TypeMismatch,
                            state.source,
                            "Condition must be a boolean",
                        )
                        .with_observation(Observation::with_help(
                            block.condition.segment(),
                            format!(
                                "Type `{}` cannot be used as a condition",
                                exploration.get_type(condition.ty).unwrap()
                            ),
                        )),
                    );
                    condition
                }
            };
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
                .map(|expr| {
                    let expr = ascribe_types(exploration, relations, diagnostics, env, expr, state);
                    convert_into_string(expr, exploration, diagnostics, state)
                })
                .collect::<Vec<_>>();
            TypedExpr {
                kind: ExprKind::ProcessCall(args),
                ty: EXIT_CODE,
                segment: call.segment(),
            }
        }
        Expr::ProgrammaticCall(call) => {
            let arguments = call
                .arguments
                .iter()
                .map(|expr| ascribe_types(exploration, relations, diagnostics, env, expr, state))
                .collect::<Vec<_>>();
            let symbol = env
                .get_raw_symbol(call.segment.clone())
                .expect("Environment has not tracked the symbol for programmatic call");
            let (definition, return_type) = type_call(
                call,
                &arguments,
                symbol,
                diagnostics,
                exploration,
                relations,
                state,
            );
            TypedExpr {
                kind: ExprKind::FunctionCall {
                    name: call.name.to_owned(),
                    arguments,
                    definition,
                },
                ty: return_type,
                segment: call.segment.clone(),
            }
        }
        Expr::MethodCall(method) => {
            let callee = ascribe_types(
                exploration,
                relations,
                diagnostics,
                env,
                &method.source,
                state,
            );
            let arguments = method
                .arguments
                .iter()
                .map(|expr| ascribe_types(exploration, relations, diagnostics, env, expr, state))
                .collect::<Vec<_>>();
            let method_type =
                type_method(method, &callee, &arguments, diagnostics, exploration, state);
            TypedExpr {
                kind: ExprKind::MethodCall {
                    callee: Box::new(callee),
                    arguments,
                    definition: method_type
                        .map(|method| method.definition)
                        .unwrap_or(Definition::error()),
                },
                ty: method_type
                    .map(|method| method.return_type)
                    .unwrap_or(ERROR),
                segment: method.segment.clone(),
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
    use crate::relations::{NativeObjectId, Symbol};
    use crate::resolve_all;
    use crate::types::ty::Type;
    use context::source::Source;
    use context::str_find::{find_in, find_in_nth};
    use parser::parse_trusted;
    use pretty_assertions::assert_eq;

    fn extract(source: Source) -> Result<(Typing, TypedExpr), Vec<Diagnostic>> {
        let typing = Typing::with_lang();
        let name = Name::new(source.name);
        let result = resolve_all(
            name.clone(),
            &mut StaticImporter::new([(name, source)], parse_trusted),
        );
        let mut diagnostics = result.diagnostics;
        assert_eq!(diagnostics, vec![]);
        let typed = apply_types(&result.engine, &result.relations, &mut diagnostics);
        let expr = typed.get_user(SourceObjectId(0)).unwrap();
        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }
        Ok((typing, expr.expression.clone()))
    }

    pub(crate) fn extract_expr(source: Source) -> Result<Vec<TypedExpr>, Vec<Diagnostic>> {
        extract(source).map(|(_, expr)| {
            if let ExprKind::Block(exprs) = expr.kind {
                exprs
            } else {
                unreachable!()
            }
        })
    }

    pub(crate) fn extract_type(source: Source) -> Result<Type, Vec<Diagnostic>> {
        let (typing, expr) = extract(source)?;
        Ok(typing.get_type(expr.ty).unwrap().clone())
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
    fn var_assign_of_same_type() {
        let res = extract_type(Source::unknown("var l = 1; l = 2"));
        assert_eq!(res, Ok(Type::Nothing));
    }

    #[test]
    fn val_cannot_reassign() {
        let content = "val l = 1; l = 2";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::CannotReassign,
                SourceObjectId(0),
                "Cannot assign twice to immutable variable `l`",
            )
            .with_observation(Observation::with_help(
                find_in(content, "l = 2"),
                "Assignment happens here",
            ))])
        );
    }

    #[test]
    fn cannot_assign_different_type() {
        let content = "var p = 1; p = 'a'";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "Cannot assign a value of type `String` to something of type `Int`",
            )
            .with_observation(Observation::with_help(
                find_in(content, "p = 'a'"),
                "Assignment happens here",
            ))])
        );
    }

    #[test]
    fn no_implicit_string_conversion() {
        let content = "var str: String = 'test'; str = 4";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "Cannot assign a value of type `Int` to something of type `String`",
            )
            .with_observation(Observation::with_help(
                find_in(content, "str = 4"),
                "Assignment happens here",
            ))])
        );
    }

    #[test]
    fn cannot_assign_to_function() {
        let content = "fun a() -> Int = 1; a = 'a'";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "Cannot assign a value of type `String` to something of type `fun#1`",
            )
            .with_observation(Observation::with_help(
                find_in(content, "a = 'a'"),
                "Assignment happens here",
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
    fn unknown_type_in_cast() {
        let content = "4 as Imaginary";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::UnknownType,
                SourceObjectId(0),
                "Unknown type annotation",
            )
            .with_observation(Observation::with_help(
                find_in(content, "Imaginary"),
                "Not found in scope",
            ))])
        );
    }

    #[test]
    fn incompatible_cast() {
        let content = "val n = 'a' as Int";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::IncompatibleCast,
                SourceObjectId(0),
                "Casting `String` as `Int` is invalid",
            )
            .with_observation(Observation::with_help(
                find_in(content, "'a' as Int"),
                "Incompatible cast",
            ))])
        );
    }

    #[test]
    fn string_template() {
        let res = extract_type(Source::unknown("val m = 5; val test = \"m = $m\"; $test"));
        assert_eq!(res, Ok(Type::String));
    }

    #[test]
    fn function_return_type() {
        let res = extract_type(Source::unknown("fun one() -> Int = 1\none()"));
        assert_eq!(res, Ok(Type::Int));
    }

    #[test]
    fn local_type_only_at_end_of_block() {
        let content = "fun test() -> Int = {if false; 5; else {}; 4}; test()";
        let res = extract_type(Source::unknown(content));
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
            .with_help("Add -> Float to the function declaration")])
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
            .with_help(
                "Try adding an explicit return type to the function"
            )])
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
            .with_help(
                "Try adding an explicit return type to the function"
            )])
        );
    }

    #[test]
    fn conversions() {
        let content = "val n = 75;val j = $n as Float\ngrep $n 4.2";
        let res = extract_expr(Source::unknown(content));
        assert_eq!(
            res,
            Ok(vec![
                TypedExpr {
                    kind: ExprKind::Declare {
                        identifier: 0,
                        value: Some(Box::new(TypedExpr {
                            kind: ExprKind::Literal(75.into()),
                            ty: INT,
                            segment: find_in(content, "75"),
                        })),
                    },
                    ty: NOTHING,
                    segment: find_in(content, "val n = 75"),
                },
                TypedExpr {
                    kind: ExprKind::Declare {
                        identifier: 1,
                        value: Some(Box::new(TypedExpr {
                            kind: ExprKind::Convert {
                                inner: Box::new(TypedExpr {
                                    kind: ExprKind::Reference(Symbol::Local(0)),
                                    ty: INT,
                                    segment: find_in(content, "$n"),
                                }),
                                into: FLOAT,
                            },
                            ty: FLOAT,
                            segment: find_in(content, "$n as Float"),
                        })),
                    },
                    ty: NOTHING,
                    segment: find_in(content, "val j = $n as Float"),
                },
                TypedExpr {
                    kind: ExprKind::ProcessCall(vec![
                        TypedExpr {
                            kind: ExprKind::Literal("grep".into()),
                            ty: STRING,
                            segment: find_in(content, "grep"),
                        },
                        TypedExpr {
                            kind: ExprKind::MethodCall {
                                callee: Box::new(TypedExpr {
                                    kind: ExprKind::Reference(Symbol::Local(0)),
                                    ty: INT,
                                    segment: find_in_nth(content, "$n", 1),
                                }),
                                arguments: vec![],
                                definition: Definition::Native(NativeObjectId(11)),
                            },
                            ty: STRING,
                            segment: find_in_nth(content, "$n", 1),
                        },
                        TypedExpr {
                            kind: ExprKind::MethodCall {
                                callee: Box::new(TypedExpr {
                                    kind: ExprKind::Literal(4.2.into()),
                                    ty: FLOAT,
                                    segment: find_in(content, "4.2"),
                                }),
                                arguments: vec![],
                                definition: Definition::Native(NativeObjectId(12)),
                            },
                            ty: STRING,
                            segment: find_in(content, "4.2"),
                        }
                    ]),
                    ty: EXIT_CODE,
                    segment: find_in(content, "grep $n 4.2"),
                }
            ])
        );
    }

    #[test]
    fn invalid_operand() {
        let content = "val c = 4 / 'a'; $c";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::UnknownMethod,
                SourceObjectId(0),
                "Undefined operator",
            )
            .with_observation(Observation::with_help(
                find_in(content, "4 / 'a'"),
                "No operator `div` between type `Int` and `String`"
            ))]),
        );
    }

    #[test]
    fn undefined_operator() {
        let content = "val c = 'operator' - 2.4; $c";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::UnknownMethod,
                SourceObjectId(0),
                "Undefined operator",
            )
            .with_observation(Observation::with_help(
                find_in(content, "'operator' - 2.4"),
                "No operator `sub` between type `String` and `Float`"
            ))]),
        );
    }

    #[test]
    fn valid_operator() {
        let content = "val c = 7.3 - 2.4; $c";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(Type::Float));
    }

    #[test]
    fn valid_operator_explicit_method() {
        let content = "val j = 7.3; val c = $j.sub(2.4); $c";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(Type::Float));
    }

    #[test]
    fn valid_method_but_invalid_parameter_count() {
        let content = "val n = 'test'.len(5)";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "This method takes 0 arguments but 1 was supplied",
            )
            .with_observation(Observation::with_help(
                find_in(content, ".len(5)"),
                "Method is called here"
            ))
            .with_help("The method signature is `String::len() -> Int`")])
        );
    }

    #[test]
    fn valid_method_but_invalid_parameter_types() {
        let content = "val j = 7.3; val c = $j.sub('a')";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "Type mismatch",
            )
            .with_observation(Observation::with_help(
                find_in(content, "'a'"),
                "Expected `Float`, found `String`"
            ))
            .with_observation(Observation::with_help(
                find_in(content, ".sub('a')"),
                "Arguments to this method are incorrect"
            ))])
        );
    }

    #[test]
    fn cannot_stringify_void() {
        let content = "val v = {}; grep $v 'test'";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "Cannot stringify type `Nothing`",
            )
            .with_observation(Observation::with_help(
                find_in(content, "$v"),
                "No method `to_string` on type `Nothing`"
            ))])
        );
    }

    #[test]
    fn condition_must_be_bool() {
        let content = "if 9.9 { 1 }";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                SourceObjectId(0),
                "Condition must be a boolean",
            )
            .with_observation(Observation::with_help(
                find_in(content, "9.9"),
                "Type `Float` cannot be used as a condition"
            ))])
        );
    }

    #[test]
    fn condition_command() {
        let res = extract_type(Source::unknown("if nginx -t { echo 'ok' }"));
        assert_eq!(res, Ok(Type::Nothing));
    }
}
