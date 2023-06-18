use crate::dependency::topological_sort;
use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::environment::Environment;
use crate::relations::{Definition, Relations, SourceId};
use crate::steps::typing::coercion::{check_type_annotation, unify_and_map};
use crate::steps::typing::exploration::{diagnose_unknown_type, Exploration};
use crate::steps::typing::function::{
    find_operand_implementation, infer_return, type_call, type_method, type_parameter, Return,
};
use crate::steps::typing::lower::convert_into_string;
use crate::types::ctx::{TypeContext, TypedVariable};
use crate::types::engine::{Chunk, TypedEngine};
use crate::types::hir::{
    Assignment, Conditional, Convert, Declaration, ExprKind, FunctionCall, Loop, MethodCall,
    TypedExpr,
};
use crate::types::operator::name_operator_method;
use crate::types::ty::Type;
use crate::types::{Typing, BOOL, ERROR, EXIT_CODE, FLOAT, INT, NOTHING, STRING};
use ast::call::{Call, ProgrammaticCall};
use ast::control_flow::If;
use ast::function::FunctionDeclaration;
use ast::group::Block;
use ast::operation::{BinaryOperation, BinaryOperator};
use ast::r#type::CastedExpr;
use ast::value::{Literal, LiteralValue, TemplateString};
use ast::variable::{Assign, VarDeclaration, VarKind, VarReference};
use ast::Expr;
use context::source::SourceSegmentHolder;

mod coercion;
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

fn ascribe_literal(lit: &Literal) -> TypedExpr {
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
    let var_obj = exploration
        .ctx
        .get(relations, state.source, symbol)
        .unwrap();
    let var_ty = var_obj.type_id;
    let rhs_type = rhs.ty;
    let rhs = match unify_and_map(
        rhs,
        var_ty,
        &mut exploration.typing,
        &exploration.engine,
        state,
        diagnostics,
    ) {
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
        kind: ExprKind::Assign(Assignment {
            identifier: symbol,
            rhs: Box::new(rhs),
        }),
        ty: NOTHING,
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
        ty: NOTHING,
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
        ty: expr.as_ref().map(|expr| expr.ty).unwrap_or(NOTHING),
        segment: ret.segment.clone(),
    });
    TypedExpr {
        kind: ExprKind::Return(expr),
        ty: NOTHING,
        segment: ret.segment.clone(),
    }
}

fn ascribe_function(
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
        kind: ExprKind::Declare(Declaration {
            identifier: local_id,
            value: None,
        }),
        ty: NOTHING,
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
        kind: ExprKind::MethodCall(MethodCall {
            callee: Box::new(left_expr),
            arguments: vec![right_expr],
            definition: method
                .map(|method| method.definition)
                .unwrap_or(Definition::error()),
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
        kind: ExprKind::Convert(Convert {
            inner: Box::new(expr),
            into: ty,
        }),
        ty,
        segment: casted.segment(),
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
    let condition = match unify_and_map(
        condition,
        BOOL,
        &mut exploration.typing,
        &exploration.engine,
        state,
        diagnostics,
    ) {
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
        match exploration.typing.unify(
            then.ty,
            otherwise.as_ref().map(|expr| expr.ty).unwrap_or(NOTHING),
        ) {
            Ok(ty) => {
                // Generate appropriate casts and implicits conversions
                then = unify_and_map(
                    then,
                    ty,
                    &mut exploration.typing,
                    &exploration.engine,
                    state,
                    diagnostics,
                )
                .expect("Type mismatch should already have been caught");
                otherwise = otherwise.map(|expr| {
                    unify_and_map(
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
            definition: method_type
                .map(|method| method.definition)
                .unwrap_or(Definition::error()),
        }),
        ty: method_type
            .map(|method| method.return_type)
            .unwrap_or(ERROR),
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
        Expr::While(w) => (
            Some(ascribe_types(
                exploration,
                relations,
                diagnostics,
                env,
                &w.condition,
                state.with_local_type(),
            )),
            &w.body,
        ),
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
        ty: NOTHING,
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
                source,
                format!("`{kind_name}` must be declared inside a loop"),
            )
            .with_observation(Observation::new(expr.segment())),
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
        Expr::FunctionDeclaration(fd) => ascribe_function(fd, state.source, env, exploration),
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
    use super::*;
    use crate::importer::StaticImporter;
    use crate::name::Name;
    use crate::relations::{LocalId, NativeId};
    use crate::resolve_all;
    use crate::types::hir::{Convert, MethodCall};
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
                SourceId(0),
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
                SourceId(0),
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
                SourceId(0),
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
                SourceId(0),
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
                SourceId(0),
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
                SourceId(0),
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
                SourceId(0),
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
                SourceId(0),
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
                SourceId(0),
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
                SourceId(0),
                "This function takes 1 argument but 2 were supplied",
            )
            .with_observation(Observation::with_help(
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
                SourceId(0),
                "Type mismatch",
            )
            .with_observation(Observation::with_help(
                find_in(content, "4"),
                "Expected `String`, found `Int`",
            ))
            .with_observation(Observation::with_help(
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
                SourceId(0),
                "Cannot invoke non function type",
            )
            .with_observation(Observation::with_help(
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
                SourceId(1),
                "Type mismatch",
            )
            .with_observation(Observation::with_help(
                find_in(content, "Int"),
                "Expected `Int`",
            ))
            .with_observation(Observation::with_help(
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
                SourceId(1),
                "Type mismatch",
            )
            .with_observation(Observation::with_help(find_in(content, "0"), "Found `Int`"))
            .with_observation(Observation::with_help(
                find_in(content, "String"),
                "Expected `String` because of return type",
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
    fn continue_and_break_inside_loops() {
        let content = "loop { continue }; loop { break }";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(Type::Nothing));
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
                    SourceId(0),
                    "`continue` must be declared inside a loop"
                )
                .with_observation(Observation::new(find_in(content, "continue"))),
                Diagnostic::new(
                    DiagnosticID::InvalidBreakOrContinue,
                    SourceId(0),
                    "`break` must be declared inside a loop"
                )
                .with_observation(Observation::new(find_in(content, "break")))
            ])
        );
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
                Diagnostic::new(DiagnosticID::TypeMismatch, SourceId(1), "Type mismatch")
                    .with_observation(Observation::with_help(
                        find_in(content, "return {}"),
                        "Found `Nothing`",
                    ))
                    .with_observation(return_observation.clone()),
                Diagnostic::new(DiagnosticID::TypeMismatch, SourceId(1), "Type mismatch")
                    .with_observation(Observation::with_help(find_in(content, "9"), "Found `Int`"))
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
                SourceId(1),
                "Return type inference is not supported yet",
            )
            .with_observation(Observation::with_help(
                find_in(content, "fun test(n: Float) = "),
                "No return type is specified",
            ),)
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
                SourceId(1),
                "Return type is not inferred for block functions",
            )
            .with_observation(Observation::with_help(
                find_in(content, "return 0"),
                "Returning `Int`",
            ))
            .with_observation(Observation::with_help(
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
                SourceId(1),
                "Failed to infer return type",
            )
            .with_observation(Observation::with_help(
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
                    ty: NOTHING,
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
                            kind: ExprKind::MethodCall(MethodCall {
                                callee: Box::new(TypedExpr {
                                    kind: ExprKind::Reference(LocalId(0).into()),
                                    ty: INT,
                                    segment: find_in_nth(content, "$n", 1),
                                }),
                                arguments: vec![],
                                definition: Definition::Native(NativeId(13)),
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
                                definition: Definition::Native(NativeId(14)),
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
                SourceId(0),
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
                SourceId(0),
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
                SourceId(0),
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
                SourceId(0),
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
                SourceId(0),
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
                SourceId(0),
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
