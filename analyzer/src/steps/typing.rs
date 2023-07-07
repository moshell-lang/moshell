use ast::call::{Call, Pipeline, ProgrammaticCall, RedirOp, Redirected};
use ast::control_flow::If;
use ast::function::FunctionDeclaration;
use ast::group::Block;
use ast::operation::{BinaryOperation, BinaryOperator};
use ast::r#type::CastedExpr;
use ast::substitution::Substitution;
use ast::test::Not;
use ast::value::{Literal, LiteralValue, TemplateString};
use ast::variable::{Assign, VarDeclaration, VarKind, VarReference};
use ast::Expr;
use context::source::SourceSegmentHolder;

use crate::dependency::topological_sort;
use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::environment::Environment;
use crate::relations::{Definition, Relations, SourceId};
use crate::steps::typing::coercion::{check_type_annotation, coerce_condition, convert_expression};
use crate::steps::typing::exploration::{diagnose_unknown_type, Exploration};
use crate::steps::typing::function::{
    find_operand_implementation, infer_return, type_call, type_method, type_parameter, Return,
};
use crate::steps::typing::lower::convert_into_string;
use crate::types::ctx::{TypeContext, TypedVariable};
use crate::types::engine::{Chunk, TypedEngine};
use crate::types::hir::{
    Assignment, Conditional, Convert, Declaration, ExprKind, FunctionCall, Loop, MethodCall, Redir,
    Redirect, TypedExpr,
};
use crate::types::operator::name_operator_method;
use crate::types::ty::Type;
use crate::types::{Typing, BOOL, ERROR, EXIT_CODE, FLOAT, INT, NOTHING, STRING, UNIT};

mod coercion;
mod exploration;
mod function;
mod lower;

pub fn apply_types(
    engine: &Engine,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
) -> (TypedEngine, Typing) {
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
    (exploration.engine, exploration.typing)
}

/// A state holder, used to informs the type checker about what should be
/// checked.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(self) struct TypingState {
    source: SourceId,
    local_type: bool,

    // if not in loop, `continue` and `break` will raise a diagnostic
    in_loop: bool,
}

impl TypingState {
    /// Creates a new initial state, for a script.
    fn new(source: SourceId) -> Self {
        Self {
            source,
            local_type: false,
            in_loop: false,
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

    /// Returns a new state with `in_loop` set to true
    fn with_in_loop(self) -> Self {
        Self {
            in_loop: true,
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
            let attached_env = env.parent.unwrap_or(source_id);
            for param in &func.parameters {
                let param = type_parameter(&exploration.ctx, param, attached_env);
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
                    .map(|param| type_parameter(&exploration.ctx, param, attached_env))
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

fn ascribe_literal(lit: &Literal) -> TypedExpr {
    let ty = match lit.parsed {
        LiteralValue::Int(_) => INT,
        LiteralValue::Float(_) => FLOAT,
        LiteralValue::String(_) => STRING,
        LiteralValue::Bool(_) => BOOL,
    };
    TypedExpr {
        kind: ExprKind::Literal(lit.parsed.clone()),
        ty,
        segment: lit.segment.clone(),
    }
}

fn ascribe_template_string(
    tpl: &TemplateString,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
    if tpl.parts.is_empty() {
        return TypedExpr {
            kind: ExprKind::Literal(LiteralValue::String(String::new())),
            ty: STRING,
            segment: tpl.segment(),
        };
    }
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
    let mut it = tpl.parts.iter().map(|part| {
        let typed_part = ascribe_types(
            exploration,
            relations,
            diagnostics,
            env,
            part,
            state.without_local_type(),
        );
        convert_into_string(typed_part, exploration, diagnostics, state)
    });
    let acc = it.next().unwrap();
    it.fold(acc, |acc, current| {
        let segment = current.segment.clone();
        TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(acc),
                arguments: vec![current],
                definition: plus_method,
            }),
            ty: STRING,
            segment,
        }
    })
}

fn ascribe_assign(
    assign: &Assign,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
    let rhs = ascribe_types(
        exploration,
        relations,
        diagnostics,
        env,
        &assign.value,
        state.with_local_type(),
    );
    let symbol = env.get_raw_symbol(assign.segment()).unwrap();
    let actual_type = exploration
        .get_type(
            exploration
                .ctx
                .get(relations, state.source, symbol)
                .unwrap()
                .type_id,
        )
        .unwrap();
    if actual_type.is_named() {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::TypeMismatch,
                format!(
                    "Named object `{}` cannot be assigned like a variable",
                    assign.name
                ),
            )
            .with_observation(Observation::here(
                state.source,
                assign.segment(),
                "Assignment happens here",
            )),
        );
        return rhs;
    }
    let var_obj = exploration
        .ctx
        .get(relations, state.source, symbol)
        .unwrap();
    let var_ty = var_obj.type_id;
    let rhs_type = rhs.ty;
    let rhs = match convert_expression(
        rhs,
        var_ty,
        &mut exploration.typing,
        &exploration.engine,
        state,
        diagnostics,
    ) {
        Ok(rhs) => rhs,
        Err(_) => {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    format!(
                        "Cannot assign a value of type `{}` to something of type `{}`",
                        exploration.get_type(rhs_type).unwrap(),
                        exploration.get_type(var_ty).unwrap()
                    ),
                )
                .with_observation(Observation::here(
                    state.source,
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

    if !var_obj.can_reassign {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::CannotReassign,
                format!(
                    "Cannot assign twice to immutable variable `{}`",
                    assign.name
                ),
            )
            .with_observation(Observation::here(
                state.source,
                assign.segment(),
                "Assignment happens here",
            )),
        );
    }

    TypedExpr {
        kind: ExprKind::Assign(Assignment {
            identifier: symbol,
            rhs: Box::new(rhs),
        }),
        ty: UNIT,
        segment: assign.segment(),
    }
}

fn ascribe_var_declaration(
    decl: &VarDeclaration,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
    let mut initializer = decl
        .initializer
        .as_ref()
        .map(|expr| {
            ascribe_types(
                exploration,
                relations,
                diagnostics,
                env,
                expr,
                state.with_local_type(),
            )
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
        initializer = check_type_annotation(
            exploration,
            type_annotation,
            initializer,
            diagnostics,
            state,
        );
    }
    TypedExpr {
        kind: ExprKind::Declare(Declaration {
            identifier: id,
            value: Some(Box::new(initializer)),
        }),
        ty: UNIT,
        segment: decl.segment.clone(),
    }
}

fn ascribe_var_reference(
    var: &VarReference,
    source: SourceId,
    env: &Environment,
    exploration: &Exploration,
    relations: &Relations,
) -> TypedExpr {
    let symbol = env.get_raw_symbol(var.segment.clone()).unwrap();
    let type_id = exploration
        .ctx
        .get(relations, source, symbol)
        .unwrap()
        .type_id;
    TypedExpr {
        kind: ExprKind::Reference(symbol),
        ty: type_id,
        segment: var.segment.clone(),
    }
}

fn ascribe_block(
    block: &Block,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
    let mut expressions = Vec::with_capacity(block.expressions.len());
    let mut it = block
        .expressions
        .iter()
        .filter(|expr| !matches!(expr, Expr::Use(_)))
        .peekable();
    while let Some(expr) = it.next() {
        expressions.push(ascribe_types(
            exploration,
            relations,
            diagnostics,
            env,
            expr,
            if it.peek().is_some() {
                state.without_local_type()
            } else {
                state
            },
        ));
    }
    let ty = expressions.last().map_or(UNIT, |expr| expr.ty);
    TypedExpr {
        kind: ExprKind::Block(expressions),
        ty,
        segment: block.segment.clone(),
    }
}

fn ascribe_redirected(
    redirected: &Redirected,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
    let expr = ascribe_types(
        exploration,
        relations,
        diagnostics,
        env,
        &redirected.expr,
        state,
    );
    let mut redirections = Vec::with_capacity(redirected.redirections.len());
    for redirection in &redirected.redirections {
        let operand = ascribe_types(
            exploration,
            relations,
            diagnostics,
            env,
            &redirection.operand,
            state,
        );
        let operand = if matches!(redirection.operator, RedirOp::FdIn | RedirOp::FdOut) {
            if operand.ty != INT {
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::TypeMismatch,
                        format!(
                            "File descriptor redirections must be given an integer, not `{}`",
                            exploration.get_type(operand.ty).unwrap()
                        ),
                    )
                    .with_observation(Observation::here(
                        state.source,
                        redirection.segment(),
                        "Redirection happens here",
                    )),
                );
            }
            operand
        } else {
            convert_into_string(operand, exploration, diagnostics, state)
        };
        redirections.push(Redir {
            fd: redirection.fd,
            operator: redirection.operator,
            operand: Box::new(operand),
        });
    }
    let ty = expr.ty;
    TypedExpr {
        kind: ExprKind::Redirect(Redirect {
            expression: Box::new(expr),
            redirections,
        }),
        ty,
        segment: redirected.segment(),
    }
}

fn ascribe_pipeline(
    pipeline: &Pipeline,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
    let mut commands = Vec::with_capacity(pipeline.commands.len());
    for command in &pipeline.commands {
        commands.push(ascribe_types(
            exploration,
            relations,
            diagnostics,
            env,
            command,
            state,
        ));
    }
    let ty = commands.last().map_or(NOTHING, |command| command.ty);
    TypedExpr {
        kind: ExprKind::Pipeline(commands),
        ty,
        segment: pipeline.segment(),
    }
}

fn ascribe_substitution(
    substitution: &Substitution,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
    let commands = substitution
        .underlying
        .expressions
        .iter()
        .map(|command| ascribe_types(exploration, relations, diagnostics, env, command, state))
        .collect::<Vec<_>>();
    TypedExpr {
        kind: ExprKind::Capture(commands),
        ty: STRING,
        segment: substitution.segment(),
    }
}

fn ascribe_return(
    ret: &ast::function::Return,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
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
        ty: expr.as_ref().map_or(UNIT, |expr| expr.ty),
        segment: ret.segment.clone(),
    });
    TypedExpr {
        kind: ExprKind::Return(expr),
        ty: NOTHING,
        segment: ret.segment.clone(),
    }
}

fn ascribe_function_declaration(
    fun: &FunctionDeclaration,
    source: SourceId,
    env: &Environment,
    exploration: &mut Exploration,
) -> TypedExpr {
    let declaration = env.get_raw_env(fun.segment.clone()).unwrap();
    let type_id = exploration
        .typing
        .add_type(Type::Function(Definition::User(declaration)));
    let local_id = exploration.ctx.push_local_type(source, type_id);

    // Forward declare the function
    let parameters = fun
        .parameters
        .iter()
        .map(|param| type_parameter(&exploration.ctx, param, source))
        .collect::<Vec<_>>();
    let return_type = fun
        .return_type
        .as_ref()
        .map_or(UNIT, |ty| exploration.ctx.resolve(ty).unwrap_or(ERROR));

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
        kind: ExprKind::Declare(Declaration {
            identifier: local_id,
            value: None,
        }),
        ty: UNIT,
        segment: fun.segment.clone(),
    }
}

fn ascribe_binary(
    bin: &BinaryOperation,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
    let left_expr = ascribe_types(exploration, relations, diagnostics, env, &bin.left, state);
    let right_expr = ascribe_types(exploration, relations, diagnostics, env, &bin.right, state);
    let name = name_operator_method(bin.op);
    let method = exploration
        .engine
        .get_methods(left_expr.ty, name)
        .and_then(|methods| find_operand_implementation(methods, &right_expr));
    let ty = match method {
        Some(method) => method.return_type,
        _ => {
            diagnostics.push(
                Diagnostic::new(DiagnosticID::UnknownMethod, "Undefined operator")
                    .with_observation(Observation::here(
                        state.source,
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
        kind: ExprKind::MethodCall(MethodCall {
            callee: Box::new(left_expr),
            arguments: vec![right_expr],
            definition: method.map_or(Definition::error(), |method| method.definition),
        }),
        ty,
        segment: bin.segment(),
    }
}

fn ascribe_casted(
    casted: &CastedExpr,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
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
    } else if expr.ty.is_ok() && exploration.typing.convert_description(ty, expr.ty).is_err() {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::IncompatibleCast,
                format!(
                    "Casting `{}` as `{}` is invalid",
                    exploration.get_type(expr.ty).unwrap(),
                    exploration.get_type(ty).unwrap()
                ),
            )
            .with_observation(Observation::here(
                state.source,
                casted.segment(),
                "Incompatible cast",
            )),
        );
    }
    TypedExpr {
        kind: ExprKind::Convert(Convert {
            inner: Box::new(expr),
            into: ty,
        }),
        ty,
        segment: casted.segment(),
    }
}

fn ascribe_not(
    not: &Not,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
    let expr = ascribe_types(
        exploration,
        relations,
        diagnostics,
        env,
        &not.underlying,
        state.with_local_type(),
    );
    let not_method = exploration
        .engine
        .get_method_exact(BOOL, "not", &[], BOOL)
        .expect("A Bool should be invertible");
    match convert_expression(
        expr,
        BOOL,
        &mut exploration.typing,
        &exploration.engine,
        state,
        diagnostics,
    ) {
        Ok(expr) => TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(expr),
                arguments: vec![],
                definition: not_method.definition,
            }),
            ty: not_method.return_type,
            segment: not.segment(),
        },
        Err(expr) => {
            diagnostics.push(
                Diagnostic::new(DiagnosticID::TypeMismatch, "Cannot invert type").with_observation(
                    Observation::here(
                        state.source,
                        not.segment(),
                        format!(
                            "Cannot invert non-boolean type `{}`",
                            exploration.get_type(expr.ty).unwrap()
                        ),
                    ),
                ),
            );
            expr
        }
    }
}

fn ascribe_if(
    block: &If,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
    let condition = ascribe_types(
        exploration,
        relations,
        diagnostics,
        env,
        &block.condition,
        state,
    );
    let condition = coerce_condition(condition, exploration, state, diagnostics);
    let mut then = ascribe_types(
        exploration,
        relations,
        diagnostics,
        env,
        &block.success_branch,
        state,
    );
    let mut otherwise = block
        .fail_branch
        .as_ref()
        .map(|expr| ascribe_types(exploration, relations, diagnostics, env, expr, state));
    let ty = if state.local_type {
        match exploration
            .typing
            .convert_many([then.ty, otherwise.as_ref().map_or(UNIT, |expr| expr.ty)])
        {
            Ok(ty) => {
                // Generate appropriate casts and implicits conversions
                then = convert_expression(
                    then,
                    ty,
                    &mut exploration.typing,
                    &exploration.engine,
                    state,
                    diagnostics,
                )
                .expect("Type mismatch should already have been caught");
                otherwise = otherwise.map(|expr| {
                    convert_expression(
                        expr,
                        ty,
                        &mut exploration.typing,
                        &exploration.engine,
                        state,
                        diagnostics,
                    )
                    .expect("Type mismatch should already have been caught")
                });
                ty
            }
            Err(_) => {
                let mut diagnostic = Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    "`if` and `else` have incompatible types",
                )
                .with_observation(Observation::here(
                    state.source,
                    block.success_branch.segment(),
                    format!("Found `{}`", exploration.get_type(then.ty).unwrap()),
                ));
                if let Some(otherwise) = &otherwise {
                    diagnostic = diagnostic.with_observation(Observation::here(
                        state.source,
                        otherwise.segment(),
                        format!("Found `{}`", exploration.get_type(otherwise.ty).unwrap()),
                    ));
                }
                diagnostics.push(diagnostic);
                ERROR
            }
        }
    } else {
        UNIT
    };
    TypedExpr {
        kind: ExprKind::Conditional(Conditional {
            condition: Box::new(condition),
            then: Box::new(then),
            otherwise: otherwise.map(Box::new),
        }),
        ty,
        segment: block.segment.clone(),
    }
}

fn ascribe_call(
    call: &Call,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
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

fn ascribe_pfc(
    call: &ProgrammaticCall,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
    let arguments = call
        .arguments
        .iter()
        .map(|expr| ascribe_types(exploration, relations, diagnostics, env, expr, state))
        .collect::<Vec<_>>();
    let symbol = env
        .get_raw_symbol(call.segment.clone())
        .expect("Environment has not tracked the symbol for programmatic call");
    let function_match = type_call(
        call,
        arguments,
        symbol,
        diagnostics,
        exploration,
        relations,
        state,
    );
    TypedExpr {
        kind: ExprKind::FunctionCall(FunctionCall {
            arguments: function_match.arguments,
            definition: function_match.definition,
        }),
        ty: function_match.return_type,
        segment: call.segment.clone(),
    }
}

fn ascribe_method_call(
    method: &ast::call::MethodCall,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
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
    let method_type = type_method(method, &callee, &arguments, diagnostics, exploration, state);
    TypedExpr {
        kind: ExprKind::MethodCall(MethodCall {
            callee: Box::new(callee),
            arguments,
            definition: method_type.map_or(Definition::error(), |method| method.definition),
        }),
        ty: method_type.map_or(ERROR, |method| method.return_type),
        segment: method.segment.clone(),
    }
}

fn ascribe_loop(
    loo: &Expr,
    exploration: &mut Exploration,
    relations: &Relations,
    diagnostics: &mut Vec<Diagnostic>,
    env: &Environment,
    state: TypingState,
) -> TypedExpr {
    let (condition, body) = match loo {
        Expr::While(w) => {
            let condition = ascribe_types(
                exploration,
                relations,
                diagnostics,
                env,
                &w.condition,
                state.with_local_type(),
            );
            (
                Some(coerce_condition(condition, exploration, state, diagnostics)),
                &w.body,
            )
        }
        Expr::Loop(l) => (None, &l.body),
        _ => unreachable!("Expression is not a loop"),
    };
    let body = ascribe_types(
        exploration,
        relations,
        diagnostics,
        env,
        body,
        state.without_local_type().with_in_loop(),
    );

    TypedExpr {
        kind: ExprKind::ConditionalLoop(Loop {
            condition: condition.map(Box::new),
            body: Box::new(body),
        }),
        segment: loo.segment(),
        ty: UNIT,
    }
}

fn ascribe_continue_or_break(
    expr: &Expr,
    diagnostics: &mut Vec<Diagnostic>,
    source: SourceId,
    in_loop: bool,
) -> TypedExpr {
    let (kind, kind_name) = match expr {
        Expr::Continue(_) => (ExprKind::Continue, "continue"),
        Expr::Break(_) => (ExprKind::Break, "break"),
        _ => panic!("e is not a loop"),
    };
    if !in_loop {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::InvalidBreakOrContinue,
                format!("`{kind_name}` must be declared inside a loop"),
            )
            .with_observation((source, expr.segment()).into()),
        );
    }
    TypedExpr {
        kind,
        ty: NOTHING,
        segment: expr.segment(),
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
        Expr::FunctionDeclaration(fd) => {
            ascribe_function_declaration(fd, state.source, env, exploration)
        }
        Expr::Literal(lit) => ascribe_literal(lit),
        Expr::TemplateString(tpl) => {
            ascribe_template_string(tpl, exploration, relations, diagnostics, env, state)
        }
        Expr::Assign(assign) => {
            ascribe_assign(assign, exploration, relations, diagnostics, env, state)
        }
        Expr::VarDeclaration(decl) => {
            ascribe_var_declaration(decl, exploration, relations, diagnostics, env, state)
        }
        Expr::VarReference(var) => {
            ascribe_var_reference(var, state.source, env, exploration, relations)
        }
        Expr::If(block) => ascribe_if(block, exploration, relations, diagnostics, env, state),
        Expr::Call(call) => ascribe_call(call, exploration, relations, diagnostics, env, state),
        Expr::ProgrammaticCall(call) => {
            ascribe_pfc(call, exploration, relations, diagnostics, env, state)
        }
        Expr::MethodCall(method) => {
            ascribe_method_call(method, exploration, relations, diagnostics, env, state)
        }
        Expr::Block(b) => ascribe_block(b, exploration, relations, diagnostics, env, state),
        Expr::Redirected(redirected) => {
            ascribe_redirected(redirected, exploration, relations, diagnostics, env, state)
        }
        Expr::Pipeline(pipeline) => {
            ascribe_pipeline(pipeline, exploration, relations, diagnostics, env, state)
        }
        Expr::Substitution(subst) => {
            ascribe_substitution(subst, exploration, relations, diagnostics, env, state)
        }
        Expr::Return(r) => ascribe_return(r, exploration, relations, diagnostics, env, state),
        Expr::Parenthesis(paren) => ascribe_types(
            exploration,
            relations,
            diagnostics,
            env,
            &paren.expression,
            state,
        ),
        Expr::Binary(bo) => ascribe_binary(bo, exploration, relations, diagnostics, env, state),
        Expr::Casted(casted) => {
            ascribe_casted(casted, exploration, relations, diagnostics, env, state)
        }
        Expr::Test(test) => ascribe_types(
            exploration,
            relations,
            diagnostics,
            env,
            &test.expression,
            state,
        ),
        Expr::Not(not) => ascribe_not(not, exploration, relations, diagnostics, env, state),
        e @ (Expr::While(_) | Expr::Loop(_)) => {
            ascribe_loop(e, exploration, relations, diagnostics, env, state)
        }
        e @ (Expr::Continue(_) | Expr::Break(_)) => {
            ascribe_continue_or_break(e, diagnostics, state.source, state.in_loop)
        }
        _ => todo!("{expr:?}"),
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use context::source::Source;
    use context::str_find::{find_in, find_in_nth};
    use parser::parse_trusted;

    use crate::importer::StaticImporter;
    use crate::name::Name;
    use crate::relations::{LocalId, NativeId};
    use crate::resolve_all;
    use crate::types::hir::{Convert, MethodCall};
    use crate::types::ty::Type;

    use super::*;

    fn extract(source: Source) -> Result<(Typing, TypedExpr), Vec<Diagnostic>> {
        let typing = Typing::with_lang();
        let name = Name::new(source.name);
        let result = resolve_all(
            name.clone(),
            &mut StaticImporter::new([(name, source)], parse_trusted),
        );
        let mut diagnostics = result.diagnostics;
        assert_eq!(diagnostics, vec![]);

        let (typed, _) = apply_types(&result.engine, &result.relations, &mut diagnostics);
        let expr = typed.get_user(SourceId(0)).unwrap();
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
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn coerce_type_annotation() {
        let res = extract_type(Source::unknown("val a: Float = 4"));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn no_coerce_type_annotation() {
        let content = "val a: Int = 1.6";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                "Type mismatch",
            )
            .with_observation(Observation::context(
                SourceId(0),
                find_in(content, "Int"),
                "Expected `Int`",
            ))
            .with_observation(Observation::here(
                SourceId(0),
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
                "Unknown type annotation",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "ABC"),
                "Not found in scope",
            ))])
        );
    }

    #[test]
    fn var_assign_of_same_type() {
        let res = extract_type(Source::unknown("var l = 1; l = 2"));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn val_cannot_reassign() {
        let content = "val l = 1; l = 2";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::CannotReassign,
                "Cannot assign twice to immutable variable `l`",
            )
            .with_observation(Observation::here(
                SourceId(0),
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
                "Cannot assign a value of type `String` to something of type `Int`",
            )
            .with_observation(Observation::here(
                SourceId(0),
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
                "Cannot assign a value of type `Int` to something of type `String`",
            )
            .with_observation(Observation::here(
                SourceId(0),
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
                "Named object `a` cannot be assigned like a variable",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "a = 'a'"),
                "Assignment happens here",
            ))])
        );
    }

    #[test]
    fn condition_same_type() {
        let res = extract_type(Source::unknown("if true; 1; else 2"));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn condition_different_type() {
        let res = extract_type(Source::unknown("if false; 4.7; else {}"));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn condition_different_type_local_return() {
        let content = "var n: Int = {if false; 4.7; else {}}";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                "`if` and `else` have incompatible types",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "4.7"),
                "Found `Float`",
            ))
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "{}"),
                "Found `Unit`",
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
                "Unknown type annotation",
            )
            .with_observation(Observation::here(
                SourceId(0),
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
                "Casting `String` as `Int` is invalid",
            )
            .with_observation(Observation::here(
                SourceId(0),
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
                "This function takes 1 argument but 2 were supplied",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "square(9, 9)"),
                "Function is called here",
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
                "Type mismatch",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "4"),
                "Expected `String`, found `Int`",
            ))
            .with_observation(Observation::context(
                SourceId(0),
                find_in(content, "str: String"),
                "Parameter is declared here",
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
                "Cannot invoke non function type",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "test()"),
                "Call expression requires function, found `Int`",
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
                "Type mismatch",
            )
            .with_observation(Observation::context(
                SourceId(1),
                find_in(content, "Int"),
                "Expected `Int`",
            ))
            .with_observation(Observation::here(
                SourceId(1),
                find_in(content, "$a"),
                "Found `String`",
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
                "Type mismatch",
            )
            .with_observation(Observation::here(
                SourceId(1),
                find_in(content, "0"),
                "Found `Int`"
            ))
            .with_observation(Observation::context(
                SourceId(1),
                find_in(content, "String"),
                "Expected `String` because of return type",
            ))])
        );
    }

    #[test]
    fn explicit_valid_return() {
        let content = "fun some() -> Int = return 20";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn continue_and_break_inside_loops() {
        let content = "loop { continue }; loop { break }";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn continue_or_break_outside_loop() {
        let content = "continue; break";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![
                Diagnostic::new(
                    DiagnosticID::InvalidBreakOrContinue,
                    "`continue` must be declared inside a loop"
                )
                .with_observation((SourceId(0), find_in(content, "continue")).into()),
                Diagnostic::new(
                    DiagnosticID::InvalidBreakOrContinue,
                    "`break` must be declared inside a loop"
                )
                .with_observation((SourceId(0), find_in(content, "break")).into())
            ])
        );
    }

    #[test]
    fn explicit_valid_return_mixed() {
        let content = "fun some() -> Int = {\nif true; return 5; 9\n}";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn explicit_invalid_return() {
        let content = "fun some() -> String = {if true; return {}; 9}";
        let res = extract_type(Source::unknown(content));
        let return_observation = Observation::context(
            SourceId(1),
            find_in(content, "String"),
            "Expected `String` because of return type",
        );
        assert_eq!(
            res,
            Err(vec![
                Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch")
                    .with_observation(Observation::here(
                        SourceId(1),
                        find_in(content, "return {}"),
                        "Found `Unit`",
                    ))
                    .with_observation(return_observation.clone()),
                Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch")
                    .with_observation(Observation::here(
                        SourceId(1),
                        find_in(content, "9"),
                        "Found `Int`"
                    ))
                    .with_observation(return_observation),
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
                "Return type inference is not supported yet",
            )
            .with_observation(Observation::here(
                SourceId(1),
                find_in(content, "fun test(n: Float) = "),
                "No return type is specified",
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
                "Return type is not inferred for block functions",
            )
            .with_observation(Observation::here(
                SourceId(1),
                find_in(content, "return 0"),
                "Returning `Int`",
            ))
            .with_observation(Observation::here(
                SourceId(1),
                find_in(content, "$n"),
                "Returning `Float`",
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
                "Failed to infer return type",
            )
            .with_observation(Observation::here(
                SourceId(1),
                find_in(content, "fun test() = if false; return 5; else {}"),
                "This function returns multiple types",
            ))
            .with_help(
                "Try adding an explicit return type to the function"
            )])
        );
    }

    #[test]
    fn conversions() {
        let content = "val n = 75 + 1;val j = $n as Float\ngrep $n 4.2";
        let res = extract_expr(Source::unknown(content));
        assert_eq!(
            res,
            Ok(vec![
                TypedExpr {
                    kind: ExprKind::Declare(Declaration {
                        identifier: LocalId(0),
                        value: Some(Box::new(TypedExpr {
                            kind: ExprKind::MethodCall(MethodCall {
                                callee: Box::new(TypedExpr {
                                    kind: ExprKind::Literal(75.into()),
                                    ty: INT,
                                    segment: find_in(content, "75"),
                                }),
                                arguments: vec![TypedExpr {
                                    kind: ExprKind::Literal(1.into()),
                                    ty: INT,
                                    segment: find_in(content, "1"),
                                }],
                                definition: Definition::Native(NativeId(1)),
                            }),
                            ty: INT,
                            segment: find_in(content, "75 + 1"),
                        })),
                    }),
                    ty: UNIT,
                    segment: find_in(content, "val n = 75 + 1"),
                },
                TypedExpr {
                    kind: ExprKind::Declare(Declaration {
                        identifier: LocalId(1),
                        value: Some(Box::new(TypedExpr {
                            kind: ExprKind::Convert(Convert {
                                inner: Box::new(TypedExpr {
                                    kind: ExprKind::Reference(LocalId(0).into()),
                                    ty: INT,
                                    segment: find_in(content, "$n"),
                                }),
                                into: FLOAT,
                            }),
                            ty: FLOAT,
                            segment: find_in(content, "$n as Float"),
                        })),
                    }),
                    ty: UNIT,
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
                            kind: ExprKind::MethodCall(MethodCall {
                                callee: Box::new(TypedExpr {
                                    kind: ExprKind::Reference(LocalId(0).into()),
                                    ty: INT,
                                    segment: find_in_nth(content, "$n", 1),
                                }),
                                arguments: vec![],
                                definition: Definition::Native(NativeId(29)),
                            }),
                            ty: STRING,
                            segment: find_in_nth(content, "$n", 1),
                        },
                        TypedExpr {
                            kind: ExprKind::MethodCall(MethodCall {
                                callee: Box::new(TypedExpr {
                                    kind: ExprKind::Literal(4.2.into()),
                                    ty: FLOAT,
                                    segment: find_in(content, "4.2"),
                                }),
                                arguments: vec![],
                                definition: Definition::Native(NativeId(30)),
                            }),
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
                "Undefined operator",
            )
            .with_observation(Observation::here(
                SourceId(0),
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
                "Undefined operator",
            )
            .with_observation(Observation::here(
                SourceId(0),
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
                "This method takes 0 arguments but 1 was supplied",
            )
            .with_observation(Observation::here(
                SourceId(0),
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
                "Type mismatch",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "'a'"),
                "Expected `Float`, found `String`"
            ))
            .with_observation(Observation::context(
                SourceId(0),
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
                "Cannot stringify type `Unit`",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "$v"),
                "No method `to_string` on type `Unit`"
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
                "Condition must be a boolean",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "9.9"),
                "Type `Float` cannot be used as a condition"
            ))])
        );
    }

    #[test]
    fn condition_previous_error() {
        let content = "if [ 9.9 % 3.3 ] { echo 'ok' }";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::UnknownMethod,
                "Undefined operator",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "9.9 % 3.3"),
                "No operator `mod` between type `Float` and `Float`"
            ))])
        );
    }

    #[test]
    fn operation_and_test() {
        let content = "val m = 101; val is_even = $m % 2 == 0; $is_even";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(Type::Bool));
    }

    #[test]
    fn condition_command() {
        let res = extract_type(Source::unknown("if nginx -t { echo 'ok' }"));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn condition_invert_command() {
        let res = extract_type(Source::unknown("if ! nginx -t { echo 'invalid config' }"));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn cannot_invert_string() {
        let content = "val s = 'test'; val is_empty = !$s";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                "Cannot invert type",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "!$s"),
                "Cannot invert non-boolean type `String`"
            ))])
        );
    }

    #[test]
    fn no_cumulative_errors() {
        let content = "var p = 'text' % 9; val r = $p.foo(); p = 4";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::UnknownMethod,
                "Undefined operator",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "'text' % 9"),
                "No operator `mod` between type `String` and `Int`"
            ))])
        );
    }

    #[test]
    fn redirect_to_string() {
        let content = "val file = '/tmp/file'; cat /etc/passwd > $file 2>&1";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(Type::ExitCode));
    }

    #[test]
    fn redirect_to_non_string() {
        let content = "val file = {}; cat /etc/passwd > $file";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                "Cannot stringify type `Unit`",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, "$file"),
                "No method `to_string` on type `Unit`"
            ))])
        );
    }

    #[test]
    fn redirect_to_string_fd() {
        let content = "grep 'test' >&matches";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                "File descriptor redirections must be given an integer, not `String`",
            )
            .with_observation(Observation::here(
                SourceId(0),
                find_in(content, ">&matches"),
                "Redirection happens here"
            ))])
        );
    }
}
