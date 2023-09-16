use ast::call::{Call, Pipeline, ProgrammaticCall, RedirOp, Redirected};
use ast::control_flow::If;
use ast::function::FunctionDeclaration;
use ast::group::Block;
use ast::operation::{BinaryOperation, BinaryOperator, UnaryOperation, UnaryOperator};
use ast::r#type::CastedExpr;
use ast::substitution::Substitution;
use ast::value::{Literal, LiteralValue, TemplateString};
use ast::variable::{Assign, VarDeclaration, VarKind, VarReference};
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};

use crate::dependency::topological_sort;
use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::reef::{Externals, ReefId};
use crate::relations::{Definition, Relations, SourceId, SymbolRef};
use crate::steps::typing::coercion::{
    check_type_annotation, coerce_condition, convert_description, convert_expression, convert_many,
    resolve_type_annotation,
};
use crate::steps::typing::exploration::{Exploration, Links};
use crate::steps::typing::function::{
    find_operand_implementation, infer_return, type_call, type_method, type_parameter, Return,
};
use crate::steps::typing::lower::convert_into_string;
use crate::types::ctx::{TypeContext, TypedVariable};
use crate::types::engine::{Chunk, TypedEngine};
use crate::types::hir::{
    Assignment, Conditional, Convert, Declaration, ExprKind, FunctionCall, Loop, MethodCall, Redir,
    Redirect, TypedExpr, Var,
};
use crate::types::operator::name_operator_method;
use crate::types::ty::{Type, TypeRef};
use crate::types::{Typing, BOOL, ERROR, EXITCODE, FLOAT, INT, NOTHING, STRING, UNIT};

mod coercion;
pub mod exploration;
mod function;
mod lower;
mod view;

pub fn apply_types(
    engine: &Engine,
    relations: &Relations,
    externals: &Externals,
    diagnostics: &mut Vec<Diagnostic>,
) -> (TypedEngine, TypeContext, Typing) {
    let dependencies = relations.as_dependencies(externals.current, engine);
    let environments = topological_sort(&dependencies);

    let mut exploration = Exploration {
        type_engine: TypedEngine::new(engine.len()),
        typing: Typing::default(),
        ctx: TypeContext::default(),
        returns: Vec::new(),
        externals,
    };

    for env_id in environments {
        let entry = apply_types_to_source(&mut exploration, diagnostics, engine, relations, env_id);
        exploration.type_engine.insert(env_id, entry);
    }
    (exploration.type_engine, exploration.ctx, exploration.typing)
}

/// A state holder, used to informs the type checker about what should be
/// checked.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
struct TypingState {
    local_type: bool,

    // if not in loop, `continue` and `break` will raise a diagnostic
    in_loop: bool,
}

impl TypingState {
    /// Creates a new initial state, for a script.
    fn new() -> Self {
        Self::default()
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

fn verify_free_function(
    func: &FunctionDeclaration,
    externals: &Externals,
    func_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if func.body.is_some() {
        return;
    }

    // only first reef (after lang) can define native functions
    if externals.current == ReefId(1) {
        return;
    }

    diagnostics.push(
        Diagnostic::new(
            DiagnosticID::NoFunctionDefinition,
            "function without a body",
        )
        .with_observation(Observation::context(
            func_id,
            externals.current,
            func.segment(),
            "provide a definition for this function",
        )),
    );
}

fn apply_types_to_source(
    exploration: &mut Exploration,
    diagnostics: &mut Vec<Diagnostic>,
    engine: &Engine,
    relations: &Relations,
    source_id: SourceId,
) -> Chunk {
    let links = Links {
        source: source_id,
        engine,
        relations,
    };
    let expr = engine.get_expression(source_id).unwrap();
    exploration.prepare();
    match expr {
        Expr::FunctionDeclaration(func) => {
            let mut params = Vec::new();
            for param in &func.parameters {
                let param = type_parameter(exploration, param, links, diagnostics);
                exploration.ctx.push_local_typed(source_id, param.ty);
                params.push(param);
            }

            let typed_body = func.body.as_ref().map(|body| {
                ascribe_types(
                    exploration,
                    links,
                    diagnostics,
                    body,
                    TypingState::default().with_local_type(),
                )
            });

            let return_type =
                infer_return(func, links, typed_body.as_ref(), diagnostics, exploration);

            if let Some(body) = typed_body {
                Chunk::function(body, params, return_type)
            } else {
                verify_free_function(func, exploration.externals, source_id, diagnostics);
                Chunk::native(params, return_type)
            }
        }
        expr => Chunk::script(ascribe_types(
            exploration,
            links,
            diagnostics,
            expr,
            TypingState::new(),
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
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    if tpl.parts.is_empty() {
        return TypedExpr {
            kind: ExprKind::Literal(LiteralValue::String(String::new())),
            ty: STRING,
            segment: tpl.segment(),
        };
    }

    let lang = exploration.externals.lang();
    let plus_method = lang
        .typed_engine
        .get_method_exact(
            STRING.type_id,
            name_operator_method(BinaryOperator::Plus),
            &[STRING],
            STRING,
        )
        .expect("string type should have a concatenation method")
        .definition;

    let mut it = tpl.parts.iter().map(|part| {
        let typed_part = ascribe_types(
            exploration,
            links,
            diagnostics,
            part,
            state.without_local_type(),
        );
        convert_into_string(typed_part, exploration, diagnostics, links.source)
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
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let rhs = ascribe_types(
        exploration,
        links,
        diagnostics,
        &assign.value,
        state.with_local_type(),
    );
    let symbol = links.env().get_raw_symbol(assign.segment()).unwrap();
    let actual_type_ref = exploration
        .ctx
        .get(links.relations, links.source, symbol)
        .unwrap()
        .type_ref;

    let actual_type = exploration.get_type_ref(actual_type_ref).unwrap();
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
                links.source,
                exploration.externals.current,
                assign.segment(),
                "Assignment happens here",
            )),
        );
        return rhs;
    }
    let var_obj = exploration
        .ctx
        .get(links.relations, links.source, symbol)
        .unwrap();
    let var_ty = var_obj.type_ref;
    let rhs_type = rhs.ty;

    let rhs = match convert_expression(rhs, var_ty, exploration, links.source, diagnostics) {
        Ok(rhs) => rhs,
        Err(_) => {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    format!(
                        "Cannot assign a value of type `{}` to something of type `{}`",
                        exploration.get_type(rhs_type),
                        exploration.get_type(var_ty)
                    ),
                )
                .with_observation(Observation::here(
                    links.source,
                    exploration.externals.current,
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
                links.source,
                exploration.externals.current,
                assign.segment(),
                "Assignment happens here",
            )),
        );
    }

    let identifier = match symbol {
        SymbolRef::Local(id) => Var::Local(id),
        SymbolRef::External(id) => Var::External(
            links.relations[id]
                .state
                .expect_resolved("non resolved relation"),
        ),
    };

    TypedExpr {
        kind: ExprKind::Assign(Assignment {
            identifier,
            rhs: Box::new(rhs),
        }),
        ty: UNIT,
        segment: assign.segment(),
    }
}

fn ascribe_var_declaration(
    decl: &VarDeclaration,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let mut initializer = decl
        .initializer
        .as_ref()
        .map(|expr| {
            ascribe_types(
                exploration,
                links,
                diagnostics,
                expr,
                state.with_local_type(),
            )
        })
        .expect("Variables without initializers are not supported yet");

    if let Some(type_annotation) = &decl.var.ty {
        initializer = check_type_annotation(
            exploration,
            type_annotation,
            initializer,
            links,
            diagnostics,
        );
    }
    let id = exploration.ctx.push_local(
        links.source,
        if decl.kind == VarKind::Val {
            TypedVariable::immutable(initializer.ty)
        } else {
            TypedVariable::assignable(initializer.ty)
        },
    );
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
    var_ref: &VarReference,
    links: Links,
    exploration: &Exploration,
) -> TypedExpr {
    let symbol = links.env().get_raw_symbol(var_ref.segment()).unwrap();
    let type_ref = exploration
        .get_var(links.source, symbol, links.relations)
        .unwrap()
        .type_ref;

    let var = match symbol {
        SymbolRef::Local(id) => Var::Local(id),
        SymbolRef::External(id) => Var::External(
            links.relations[id]
                .state
                .expect_resolved("non resolved relation"),
        ),
    };

    TypedExpr {
        kind: ExprKind::Reference(var),
        ty: type_ref,
        segment: var_ref.segment.clone(),
    }
}

fn ascribe_block(
    block: &Block,
    links: Links,
    exploration: &mut Exploration,
    diagnostics: &mut Vec<Diagnostic>,
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
            links,
            diagnostics,
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
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let expr = ascribe_types(exploration, links, diagnostics, &redirected.expr, state);

    let mut redirections = Vec::with_capacity(redirected.redirections.len());
    for redirection in &redirected.redirections {
        let operand = ascribe_types(exploration, links, diagnostics, &redirection.operand, state);
        let operand = if matches!(redirection.operator, RedirOp::FdIn | RedirOp::FdOut) {
            if operand.ty != INT {
                diagnostics.push(
                    Diagnostic::new(
                        DiagnosticID::TypeMismatch,
                        format!(
                            "File descriptor redirections must be given an integer, not `{}`",
                            exploration.get_type(operand.ty)
                        ),
                    )
                    .with_observation(Observation::here(
                        links.source,
                        exploration.externals.current,
                        redirection.segment(),
                        "Redirection happens here",
                    )),
                );
            }
            operand
        } else {
            convert_into_string(operand, exploration, diagnostics, links.source)
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
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let mut commands = Vec::with_capacity(pipeline.commands.len());
    for command in &pipeline.commands {
        commands.push(ascribe_types(
            exploration,
            links,
            diagnostics,
            command,
            state,
        ));
    }
    TypedExpr {
        kind: ExprKind::Pipeline(commands),
        ty: EXITCODE,
        segment: pipeline.segment(),
    }
}

fn ascribe_substitution(
    substitution: &Substitution,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let commands = substitution
        .underlying
        .expressions
        .iter()
        .map(|command| ascribe_types(exploration, links, diagnostics, command, state))
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
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let expr = ret
        .expr
        .as_ref()
        .map(|expr| Box::new(ascribe_types(exploration, links, diagnostics, expr, state)));
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
    exploration: &mut Exploration,
    links: Links,
) -> TypedExpr {
    let declaration = links.env().get_raw_env(fun.segment()).unwrap();
    let declaration_link = links.with_source(declaration);

    let type_id = exploration
        .typing
        .add_type(Type::Function(Definition::User(declaration)));
    let type_ref = TypeRef::new(exploration.externals.current, type_id);

    let local_id = exploration.ctx.push_local_typed(links.source, type_ref);

    // Forward declare the function
    let parameters = fun
        .parameters
        .iter()
        .map(|param| type_parameter(exploration, param, declaration_link, &mut vec![])) // Silent errors
        .collect::<Vec<_>>();

    let return_type = fun.return_type.as_ref().map_or(UNIT, |ty| {
        resolve_type_annotation(exploration, declaration_link, ty, &mut Vec::new())
    }); // Silent errors

    exploration.type_engine.insert_if_absent(
        declaration,
        Chunk::function(
            TypedExpr {
                kind: ExprKind::Noop,
                ty: type_ref,
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
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let left_expr = ascribe_types(exploration, links, diagnostics, &bin.left, state);
    let right_expr = ascribe_types(exploration, links, diagnostics, &bin.right, state);
    let right_type = right_expr.ty;
    let name = name_operator_method(bin.op);

    let method = exploration
        .get_methods(left_expr.ty, name)
        .and_then(|methods| {
            find_operand_implementation(
                exploration,
                left_expr.ty.reef,
                methods,
                left_expr.ty,
                right_expr,
            )
        });
    match method {
        Some(method) => TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(left_expr),
                arguments: method.arguments,
                definition: method.definition,
            }),
            ty: method.return_type,
            segment: bin.segment(),
        },
        _ => {
            diagnostics.push(
                Diagnostic::new(DiagnosticID::UnknownMethod, "Undefined operator")
                    .with_observation(Observation::here(
                        links.source,
                        exploration.externals.current,
                        bin.segment(),
                        format!(
                            "No operator `{}` between type `{}` and `{}`",
                            name,
                            exploration.get_type(left_expr.ty),
                            exploration.get_type(right_type)
                        ),
                    )),
            );
            left_expr.poison()
        }
    }
}

fn ascribe_casted(
    casted: &CastedExpr,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let expr = ascribe_types(exploration, links, diagnostics, &casted.expr, state);

    let ty = resolve_type_annotation(exploration, links, &casted.casted_type, diagnostics);
    if ty.is_err() {
        return expr;
    }

    if expr.ty.is_ok() && convert_description(exploration, ty, expr.ty).is_err() {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::IncompatibleCast,
                format!(
                    "Casting `{}` as `{}` is invalid",
                    exploration.get_type(expr.ty),
                    exploration.get_type(ty)
                ),
            )
            .with_observation(Observation::here(
                links.source,
                exploration.externals.current,
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

fn ascribe_unary(
    unary: &UnaryOperation,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let expr = ascribe_types(
        exploration,
        links,
        diagnostics,
        &unary.expr,
        state.with_local_type(),
    );
    if expr.ty.is_err() {
        return expr;
    }

    match unary.op {
        UnaryOperator::Not => ascribe_not(expr, unary.segment(), exploration, links, diagnostics),
        UnaryOperator::Negate => {
            let method = exploration.get_method_exact(expr.ty, "neg", &[], expr.ty);

            match method {
                Some(method) => TypedExpr {
                    kind: ExprKind::MethodCall(MethodCall {
                        callee: Box::new(expr),
                        arguments: vec![],
                        definition: method.definition,
                    }),
                    ty: method.return_type,
                    segment: unary.segment(),
                },
                None => {
                    diagnostics.push(
                        Diagnostic::new(DiagnosticID::UnknownMethod, "Cannot negate type")
                            .with_observation(Observation::here(
                                links.source,
                                exploration.externals.current,
                                unary.segment(),
                                format!(
                                    "`{}` does not implement the `neg` method",
                                    exploration.get_type(expr.ty),
                                ),
                            )),
                    );
                    expr
                }
            }
        }
    }
}

fn ascribe_not(
    not: TypedExpr,
    segment: SourceSegment,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedExpr {
    let lang = exploration.externals.lang();
    let not_method = lang
        .typed_engine
        .get_method_exact(BOOL.type_id, "not", &[], BOOL)
        .expect("A Bool should be invertible");
    match convert_expression(not, BOOL, exploration, links.source, diagnostics) {
        Ok(expr) => TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(expr),
                arguments: vec![],
                definition: not_method.definition,
            }),
            ty: not_method.return_type,
            segment,
        },
        Err(expr) => {
            diagnostics.push(
                Diagnostic::new(DiagnosticID::TypeMismatch, "Cannot invert type").with_observation(
                    Observation::here(
                        links.source,
                        exploration.externals.current,
                        segment,
                        format!(
                            "Cannot invert non-boolean type `{}`",
                            exploration.get_type(expr.ty)
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
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let condition = ascribe_types(exploration, links, diagnostics, &block.condition, state);

    let current_reef = exploration.externals.current;

    let condition = coerce_condition(condition, exploration, links.source, diagnostics);
    let mut then = ascribe_types(
        exploration,
        links,
        diagnostics,
        &block.success_branch,
        state,
    );

    let mut otherwise = block
        .fail_branch
        .as_ref()
        .map(|expr| ascribe_types(exploration, links, diagnostics, expr, state));

    let ty = if state.local_type {
        match convert_many(
            exploration,
            [then.ty, otherwise.as_ref().map_or(UNIT, |expr| expr.ty)],
        ) {
            Ok(ty) => {
                // Generate appropriate casts and implicits conversions
                then = convert_expression(then, ty, exploration, links.source, diagnostics)
                    .expect("Type mismatch should already have been caught");
                otherwise = otherwise.map(|expr| {
                    convert_expression(expr, ty, exploration, links.source, diagnostics)
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
                    links.source,
                    current_reef,
                    block.success_branch.segment(),
                    format!("Found `{}`", exploration.get_type(then.ty)),
                ));
                if let Some(otherwise) = &otherwise {
                    diagnostic = diagnostic.with_observation(Observation::here(
                        links.source,
                        current_reef,
                        otherwise.segment(),
                        format!("Found `{}`", exploration.get_type(otherwise.ty)),
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
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let args = call
        .arguments
        .iter()
        .map(|expr| {
            let expr = ascribe_types(exploration, links, diagnostics, expr, state);
            convert_into_string(expr, exploration, diagnostics, links.source)
        })
        .collect::<Vec<_>>();

    TypedExpr {
        kind: ExprKind::ProcessCall(args),
        ty: EXITCODE,
        segment: call.segment(),
    }
}

fn ascribe_pfc(
    call: &ProgrammaticCall,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let arguments = call
        .arguments
        .iter()
        .map(|expr| ascribe_types(exploration, links, diagnostics, expr, state))
        .collect::<Vec<_>>();

    let function_match = type_call(call, exploration, arguments, links, diagnostics);
    TypedExpr {
        kind: ExprKind::FunctionCall(FunctionCall {
            arguments: function_match.arguments,
            definition: function_match.definition,
            reef: function_match.reef,
        }),
        ty: function_match.return_type,
        segment: call.segment.clone(),
    }
}

fn ascribe_method_call(
    method: &ast::call::MethodCall,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let mut callee = ascribe_types(exploration, links, diagnostics, &method.source, state);
    let arguments = method
        .arguments
        .iter()
        .map(|expr| ascribe_types(exploration, links, diagnostics, expr, state))
        .collect::<Vec<_>>();
    match type_method(
        method,
        &callee,
        arguments,
        diagnostics,
        exploration,
        links.source,
    ) {
        Some(fun) => TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(callee),
                arguments: fun.arguments,
                definition: fun.definition,
            }),
            ty: fun.return_type,
            segment: method.segment.clone(),
        },
        None => {
            callee.ty = ERROR;
            callee
        }
    }
}

fn ascribe_loop(
    loo: &Expr,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let (condition, body) = match loo {
        Expr::While(w) => {
            let condition = ascribe_types(
                exploration,
                links,
                diagnostics,
                &w.condition,
                state.with_local_type(),
            );
            (
                Some(coerce_condition(
                    condition,
                    exploration,
                    links.source,
                    diagnostics,
                )),
                &w.body,
            )
        }
        Expr::Loop(l) => (None, &l.body),
        _ => unreachable!("Expression is not a loop"),
    };
    let body = ascribe_types(
        exploration,
        links,
        diagnostics,
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
    current_reef: ReefId,
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
            .with_observation((source, current_reef, expr.segment()).into()),
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
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    expr: &Expr,
    state: TypingState,
) -> TypedExpr {
    match expr {
        Expr::FunctionDeclaration(fd) => ascribe_function_declaration(fd, exploration, links),
        Expr::Literal(lit) => ascribe_literal(lit),
        Expr::TemplateString(tpl) => {
            ascribe_template_string(tpl, exploration, links, diagnostics, state)
        }
        Expr::Assign(assign) => ascribe_assign(assign, exploration, links, diagnostics, state),
        Expr::VarDeclaration(decl) => {
            ascribe_var_declaration(decl, exploration, links, diagnostics, state)
        }
        Expr::VarReference(var) => ascribe_var_reference(var, links, exploration),
        Expr::If(block) => ascribe_if(block, exploration, links, diagnostics, state),
        Expr::Call(call) => ascribe_call(call, exploration, links, diagnostics, state),
        Expr::ProgrammaticCall(call) => ascribe_pfc(call, exploration, links, diagnostics, state),
        Expr::MethodCall(method) => {
            ascribe_method_call(method, exploration, links, diagnostics, state)
        }
        Expr::Block(b) => ascribe_block(b, links, exploration, diagnostics, state),
        Expr::Redirected(redirected) => {
            ascribe_redirected(redirected, exploration, links, diagnostics, state)
        }
        Expr::Pipeline(pipeline) => {
            ascribe_pipeline(pipeline, exploration, links, diagnostics, state)
        }
        Expr::Substitution(subst) => {
            ascribe_substitution(subst, exploration, links, diagnostics, state)
        }
        Expr::Return(r) => ascribe_return(r, exploration, links, diagnostics, state),
        Expr::Parenthesis(paren) => {
            ascribe_types(exploration, links, diagnostics, &paren.expression, state)
        }
        Expr::Unary(unary) => ascribe_unary(unary, exploration, links, diagnostics, state),
        Expr::Binary(bo) => ascribe_binary(bo, exploration, links, diagnostics, state),
        Expr::Casted(casted) => ascribe_casted(casted, exploration, links, diagnostics, state),
        Expr::Test(test) => ascribe_types(exploration, links, diagnostics, &test.expression, state),
        e @ (Expr::While(_) | Expr::Loop(_)) => {
            ascribe_loop(e, exploration, links, diagnostics, state)
        }
        e @ (Expr::Continue(_) | Expr::Break(_)) => ascribe_continue_or_break(
            e,
            diagnostics,
            links.source,
            exploration.externals.current,
            state.in_loop,
        ),
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
    use crate::reef::ReefId;
    use crate::relations::{LocalId, NativeId};
    use crate::resolve_all;
    use crate::types::hir::{Convert, MethodCall};
    use crate::types::ty::Type;
    use crate::types::{EXITCODE, UNIT};

    use super::*;

    fn extract(source: Source) -> Result<(Typing, TypedExpr), Vec<Diagnostic>> {
        let name = Name::new(source.name);
        let mut diagnostics = Vec::new();
        let externals = Externals::default();

        let result = resolve_all(
            name.clone(),
            &externals,
            &mut StaticImporter::new([(name, source)], parse_trusted),
            &mut diagnostics,
        );
        assert_eq!(diagnostics, vec![]);

        let (typed, _, _) = apply_types(
            &result.engine,
            &result.relations,
            &externals,
            &mut diagnostics,
        );
        let expr = typed.get_user(SourceId(0)).unwrap();
        if diagnostics.is_empty() {
            Ok((
                externals.lang().typing.clone(),
                expr.expression.clone().unwrap(),
            ))
        } else {
            Err(diagnostics)
        }
    }

    pub(crate) fn extract_expr(source: Source) -> Result<Vec<TypedExpr>, Vec<Diagnostic>> {
        extract(source).map(|(_, expr)| {
            if let ExprKind::Block(exprs) = &expr.kind {
                exprs.clone()
            } else {
                unreachable!()
            }
        })
    }

    pub(crate) fn extract_type(source: Source) -> Result<Type, Vec<Diagnostic>> {
        let (typing, expr) = extract(source)?;
        Ok(typing.get_type(expr.ty.type_id).unwrap().clone())
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
                ReefId(1),
                find_in(content, "Int"),
                "Expected `Int`",
            ))
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, "1.6"),
                "Found `Float`",
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
                ReefId(1),
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
                ReefId(1),
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
                ReefId(1),
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
                ReefId(1),
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
                ReefId(1),
                find_in(content, "4.7"),
                "Found `Float`",
            ))
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, "{}"),
                "Found `Unit`",
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
                ReefId(1),
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
                ReefId(1),
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
                ReefId(1),
                find_in(content, "4"),
                "Expected `String`, found `Int`",
            ))
            .with_observation(Observation::context(
                SourceId(1),
                ReefId(1),
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
                ReefId(1),
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
                ReefId(1),
                find_in(content, "Int"),
                "Expected `Int`",
            ))
            .with_observation(Observation::here(
                SourceId(1),
                ReefId(1),
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
                ReefId(1),
                find_in(content, "0"),
                "Found `Int`"
            ))
            .with_observation(Observation::context(
                SourceId(1),
                ReefId(1),
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
                .with_observation((SourceId(0), ReefId(1), find_in(content, "continue")).into()),
                Diagnostic::new(
                    DiagnosticID::InvalidBreakOrContinue,
                    "`break` must be declared inside a loop"
                )
                .with_observation((SourceId(0), ReefId(1), find_in(content, "break")).into())
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
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                "Type mismatch"
            )
            .with_observation(Observation::here(
                SourceId(1),
                ReefId(1),
                find_in(content, "return {}"),
                "Found `Unit`",
            ))
            .with_observation(Observation::here(
                SourceId(1),
                ReefId(1),
                find_in(content, "9"),
                "Found `Int`"
            ))
            .with_observation(Observation::context(
                SourceId(1),
                ReefId(1),
                find_in(content, "String"),
                "Expected `String` because of return type",
            ))])
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
            .with_observation(Observation::context(
                SourceId(1),
                ReefId(1),
                find_in(content, "fun test(n: Float) = "),
                "No return type is specified",
            ))
            .with_observation(Observation::here(
                SourceId(1),
                ReefId(1),
                find_in(content, "if false; 0.0; else $n"),
                "Returning `Float`",
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
                ReefId(1),
                find_in(content, "return 0"),
                "Returning `Int`",
            ))
            .with_observation(Observation::here(
                SourceId(1),
                ReefId(1),
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
                ReefId(1),
                find_in(content, "fun test() = "),
                "This function returns multiple types",
            ))
            .with_observation(Observation::here(
                SourceId(1),
                ReefId(1),
                find_in(content, "return 5"),
                "Returning `Int`",
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
                                    kind: ExprKind::Reference(Var::Local(LocalId(0))),
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
                                    kind: ExprKind::Reference(Var::Local(LocalId(0))),
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
                    ty: EXITCODE,
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
                ReefId(1),
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
                ReefId(1),
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
                ReefId(1),
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
                ReefId(1),
                find_in(content, "'a'"),
                "Expected `Float`, found `String`"
            ))
            .with_observation(Observation::context(
                SourceId(0),
                ReefId(1),
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
                ReefId(1),
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
                ReefId(1),
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
                ReefId(1),
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
                ReefId(1),
                find_in(content, "!$s"),
                "Cannot invert non-boolean type `String`"
            ))])
        );
    }

    #[test]
    fn cannot_negate_unit() {
        let content = "val opposite = -{}";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::UnknownMethod,
                "Cannot negate type",
            )
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, "-{}"),
                "`Unit` does not implement the `neg` method"
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
                ReefId(1),
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
                ReefId(1),
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
                ReefId(1),
                find_in(content, ">&matches"),
                "Redirection happens here"
            ))])
        );
    }

    #[test]
    fn use_pipeline_return() {
        let res = extract_type(Source::unknown(
            "if echo hello | grep -q test | val m = $(cat test) {}",
        ));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn use_unit_result() {
        let res = extract_type(Source::unknown(
            "fun foo() = { fun bar() = { return }; bar() }",
        ));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn correct_concretized_type() {
        let res = extract_type(Source::unknown("'Hello, world'.split(' ').push('!')"));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn access_method_in_parameter() {
        let res = extract_type(Source::unknown(
            "fun len(bytes: Vec[Int]) -> Int = $bytes.len()",
        ));
        assert_eq!(res, Ok(Type::Unit));
    }

    #[test]
    fn incorrect_generic_param() {
        let content = "'Hello, world'.split(' ').push({})";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                "Type mismatch",
            )
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, "{}"),
                "Expected `String`, found `Unit`"
            ))
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, ".push({})"),
                "Arguments to this method are incorrect"
            ))])
        );
    }

    #[test]
    fn incorrect_concretized_type() {
        let content =
            "val lines: Vec[String] = 'Hello, world'.split('\\n'); val first: Int = $lines.get(0)";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                "Type mismatch",
            )
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, "Int"),
                "Expected `Int`"
            ))
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, ".get(0)"),
                "Found `String`"
            ))])
        );
    }
    #[test]
    fn different_concrete_type() {
        let content = "val lines = 'Hello, world'.split('\\n'); val types: Vec[Float] = $lines";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                "Type mismatch",
            )
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, "Vec[Float]"),
                "Expected `Vec[Float]`"
            ))
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, "$lines"),
                "Found `Vec[String]`"
            ))])
        );
    }

    #[test]
    fn invalid_type_arguments_count() {
        let content = "fun foo(n: Int[Int]) = {}";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::InvalidTypeArguments,
                "Type `Int` were supplied 1 generic argument",
            )
            .with_observation(Observation::here(
                SourceId(1),
                ReefId(1),
                find_in(content, "Int[Int]"),
                "Expected 0 generic arguments"
            ))])
        );
    }
}
