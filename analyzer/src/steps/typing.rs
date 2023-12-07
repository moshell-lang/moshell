use std::str::FromStr;

use ast::call::{Call, Detached, Pipeline, ProgrammaticCall, RedirOp, Redirected};
use ast::control_flow::If;
use ast::function::FunctionDeclaration;
use ast::group::{Block, Subshell};
use ast::operation::{BinaryOperation, BinaryOperator, UnaryOperation, UnaryOperator};
use ast::r#type::CastedExpr;
use ast::r#use::InclusionPathItem;
use ast::range::{Iterable, Subscript};
use ast::substitution::Substitution;
use ast::value::{Literal, LiteralValue, TemplateString};
use ast::variable::{Assign, Identifier, Tilde, VarDeclaration, VarKind, VarName, VarReference};
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder};

use crate::dependency::topological_sort;
use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::reef::{Externals, ReefId};
use crate::relations::{Relations, SourceId, SymbolRef};
use crate::steps::typing::assign::{
    ascribe_assign_rhs, ascribe_assign_subscript, create_subscript,
};
use crate::steps::typing::bounds::TypesBounds;
use crate::steps::typing::coercion::{
    check_type_annotation, coerce_condition, convert_description, convert_expression, convert_many,
    resolve_type_annotation,
};
use crate::steps::typing::exploration::{Exploration, Links};
use crate::steps::typing::function::{
    declare_function, find_operand_implementation, infer_return, type_call, type_method, Return,
};
use crate::steps::typing::lower::{convert_into_string, generate_unwrap};
use crate::steps::typing::magic::{is_magic_variable_name, prepend_implicits};
use crate::steps::typing::structure::{
    ascribe_field_access, ascribe_field_assign, ascribe_struct_declaration,
};
use crate::types::builtin::{BOOL_STRUCT, STRING_STRUCT};
use crate::types::ctx::{TypeContext, TypedVariable};
use crate::types::engine::{Chunk, ChunkKind, TypedEngine};
use crate::types::hir::{
    Conditional, Convert, Declaration, ExprKind, FunctionCall, LocalAssignment, Loop, MethodCall,
    Redir, Redirect, Subprocess, TypedExpr, Var,
};
use crate::types::operator::name_operator_method;
use crate::types::ty::{FunctionDesc, Type, TypeRef};
use crate::types::{
    builtin, Typing, BOOL, ERROR, EXITCODE, FLOAT, GLOB, INT, NOTHING, PID, STRING, UNIT,
};

mod assign;
mod bounds;
mod coercion;
mod exploration;
mod function;
mod lower;
mod structure;
mod view;

pub mod magic;

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
        if let Some(entry) =
            apply_types_to_source(&mut exploration, diagnostics, engine, relations, env_id)
        {
            exploration.type_engine.insert(env_id, entry);
        }
    }
    (exploration.type_engine, exploration.ctx, exploration.typing)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
enum ExpressionValue {
    /// The value of the expression is not used
    #[default]
    Unused,
    /// The value of the expression is used but its type is not specified
    Unspecified,
    /// The value of the expression is used and is of expected type
    Expected(TypeRef),
}

/// A state holder, used to informs the type checker about what should be
/// checked.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
struct TypingState {
    // if not in loop, `continue` and `break` will raise a diagnostic
    in_loop: bool,

    local_value: ExpressionValue,
}

impl TypingState {
    /// Creates a new initial state, for a script.
    fn new() -> Self {
        Self::default()
    }

    /// Returns a new state with given expression value.
    fn with_local_value(self, v: ExpressionValue) -> Self {
        Self {
            local_value: v,
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
) -> Option<Chunk> {
    let links = Links {
        source: source_id,
        engine,
        relations,
    };
    let expr = engine.get_expression(source_id).unwrap();
    exploration.prepare();
    match expr {
        Expr::FunctionDeclaration(func) => {
            // Take any previous forward declaration if present.
            let forward_declaration = exploration.type_engine.take_user(source_id);

            let base_chunk = forward_declaration
                .unwrap_or_else(|| declare_function(func, exploration, links, diagnostics));

            let function_id = base_chunk.function_id;
            let function_type = base_chunk.function_type;

            let chunk_function = exploration.type_engine.get_function(function_id).unwrap();
            let expected_return_type = chunk_function.return_type;

            let typed_body = func.body.as_ref().map(|body| {
                ascribe_types(
                    exploration,
                    links,
                    diagnostics,
                    body,
                    TypingState::default()
                        .with_local_value(ExpressionValue::Expected(expected_return_type)),
                )
            });

            let return_type = infer_return(
                func,
                expected_return_type,
                links,
                typed_body.as_ref(),
                diagnostics,
                exploration,
            );

            // update function's return type.
            let function_mut = exploration
                .type_engine
                .get_function_mut(function_id)
                .unwrap();
            function_mut.return_type = return_type;

            match typed_body {
                Some(body) => Some(Chunk {
                    function_id,
                    function_type,
                    kind: ChunkKind::DefinedFunction(Some(body)),
                }),
                None => {
                    verify_free_function(func, exploration.externals, source_id, diagnostics);
                    Some(Chunk {
                        function_id,
                        function_type,
                        kind: ChunkKind::DeclaredFunction,
                    })
                }
            }
        }
        Expr::StructDeclaration(_) => None,
        expr => {
            exploration
                .ctx
                .init_locals(links.source, links.env().symbols.len());

            let expression =
                ascribe_types(exploration, links, diagnostics, expr, TypingState::new());

            let script_fn_id = exploration.type_engine.add_function(FunctionDesc::script());
            let script_fn_name = links.env().fqn.to_string();
            let function_type = exploration.typing.add_type(
                Type::Function(Some(links.source), script_fn_id),
                Some(script_fn_name),
            );

            let expression = prepend_implicits(expression, exploration, links);

            Some(Chunk {
                function_id: script_fn_id,
                function_type,
                kind: ChunkKind::DefinedFunction(Some(expression)),
            })
        }
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
    let (_, plus_method_id) = lang
        .typed_engine
        .get_method_exact(
            STRING_STRUCT,
            name_operator_method(BinaryOperator::Plus),
            &[STRING],
            STRING,
        )
        .expect("string type should have a concatenation method");

    let mut it = tpl.parts.iter().map(|part| {
        let typed_part = ascribe_types(
            exploration,
            links,
            diagnostics,
            part,
            state.with_local_value(ExpressionValue::Unused),
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
                function_id: plus_method_id,
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
    let symbol = links.env().get_raw_symbol(assign.left.segment());

    let actual_type_ref = symbol.map(|symbol| {
        exploration
            .ctx
            .get(links.relations, links.source, symbol)
            .unwrap()
            .type_ref
    });

    if let Expr::Subscript(sub) = assign.left.as_ref() {
        return ascribe_assign_subscript(assign, sub, exploration, links, diagnostics, state);
    }
    if let Expr::FieldAccess(field) = assign.left.as_ref() {
        return ascribe_field_assign(assign, field, exploration, links, diagnostics, state);
    }

    let rhs = ascribe_assign_rhs(
        assign,
        exploration,
        links,
        diagnostics,
        state.with_local_value(
            actual_type_ref.map_or(ExpressionValue::Unspecified, ExpressionValue::Expected),
        ),
    );

    // actual_type_ref is some if symbol is some
    let Some((symbol, actual_type_ref)) = symbol.map(|s| (s, actual_type_ref.unwrap())) else {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::InvalidAssignment,
                "Invalid left-hand side of assignment",
            )
            .with_observation(Observation::here(
                links.source,
                exploration.externals.current,
                assign.left.segment(),
                "Cannot assign to this expression",
            )),
        );
        return rhs;
    };

    let actual_type = exploration.get_type(actual_type_ref).unwrap();
    if actual_type.is_named() {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::TypeMismatch,
                if let Some(name) = assign.name() {
                    format!("Named object `{}` cannot be assigned like a variable", name)
                } else {
                    "Expression cannot be assigned like a variable".to_owned()
                },
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

    let rhs = match convert_expression(
        rhs,
        var_ty,
        &mut TypesBounds::inactive(),
        exploration,
        links.source,
        diagnostics,
    ) {
        Ok(rhs) => rhs,
        Err(_) => {
            diagnostics.push(
                Diagnostic::new(
                    DiagnosticID::TypeMismatch,
                    format!(
                        "Cannot assign a value of type `{}` to something of type `{}`",
                        exploration.new_type_view(rhs_type, &TypesBounds::inactive()),
                        exploration.new_type_view(var_ty, &TypesBounds::inactive()),
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
                if let Some(name) = assign.name() {
                    format!("Cannot assign twice to immutable variable `{}`", name)
                } else {
                    "Cannot reassign immutable expression".to_owned()
                },
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
        kind: ExprKind::LocalAssign(LocalAssignment {
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
    let ast_type_hint = decl.var.ty.as_ref();
    let type_hint =
        ast_type_hint.map(|ty| resolve_type_annotation(exploration, links, ty, diagnostics));

    let mut initializer = decl
        .initializer
        .as_ref()
        .map(|expr| {
            ascribe_types(
                exploration,
                links,
                diagnostics,
                expr,
                state.with_local_value(
                    type_hint.map_or(ExpressionValue::Unspecified, ExpressionValue::Expected),
                ),
            )
        })
        .expect("Variables without initializers are not supported yet");

    if let Some(type_ref) = type_hint {
        initializer = check_type_annotation(
            exploration,
            type_ref,
            ast_type_hint.unwrap().segment(),
            &mut TypesBounds::inactive(),
            initializer,
            links,
            diagnostics,
        );
    }

    let id = links.env().get_raw_symbol(decl.segment()).unwrap();

    let SymbolRef::Local(id) = id else {
        unreachable!()
    };

    exploration.ctx.set_local(
        links.source,
        id,
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

fn ascribe_magic_var_reference(
    var_ref: &VarReference,
    exploration: &Exploration,
    links: Links,
) -> Option<TypedExpr> {
    let var_name = var_ref.name.name();
    if !is_magic_variable_name(var_name) {
        return None;
    }

    let program_arguments_variable = links.env().get_raw_symbol(var_ref.segment()).unwrap();

    let pargs_var = match program_arguments_variable {
        SymbolRef::Local(l) => Var::Local(l),
        SymbolRef::External(e) => Var::External(
            links.relations[e]
                .state
                .expect_resolved("unresolved magic variable"),
        ),
    };

    let parg_reference_expression = TypedExpr {
        kind: ExprKind::Reference(pargs_var),
        ty: builtin::STRING_VEC, //program arguments is of type Vec[String]
        segment: var_ref.segment(),
    };

    match var_name {
        "#" => {
            let (_, len_method_id) = exploration
                .get_method_exact(parg_reference_expression.ty, "len", &[], INT)
                .expect("Vec#len(): Int method not found");

            Some(TypedExpr {
                kind: ExprKind::MethodCall(MethodCall {
                    callee: Box::new(parg_reference_expression),
                    arguments: vec![],
                    function_id: len_method_id,
                }),
                ty: INT,
                segment: var_ref.segment(),
            })
        }
        "*" | "@" => Some(parg_reference_expression),
        _ => {
            let Ok(offset) = u32::from_str(var_name) else {
                return None;
            };

            let (_, index_method_id) = exploration
                .get_method_exact(
                    parg_reference_expression.ty,
                    "[]",
                    &[INT],
                    builtin::GENERIC_PARAMETER_1,
                )
                .expect("Vec#[Int]: T method not found");

            Some(TypedExpr {
                kind: ExprKind::MethodCall(MethodCall {
                    callee: Box::new(parg_reference_expression),
                    arguments: vec![TypedExpr {
                        kind: ExprKind::Literal(LiteralValue::Int(offset as i64)),
                        ty: INT,
                        segment: var_ref.segment(),
                    }],
                    function_id: index_method_id,
                }),
                ty: STRING,
                segment: var_ref.segment(),
            })
        }
    }
}

fn ascribe_var_reference(
    var_ref: &VarReference,
    links: Links,
    exploration: &Exploration,
) -> TypedExpr {
    if let Some(magic_ref) = ascribe_magic_var_reference(var_ref, exploration, links) {
        return magic_ref;
    }

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

fn ascribe_identifier(ident: &Identifier, links: Links, exploration: &Exploration) -> TypedExpr {
    ascribe_var_reference(
        &VarReference {
            name: VarName::User(ident.name),
            segment: ident.segment.clone(),
        },
        links,
        exploration,
    )
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
                state.with_local_value(ExpressionValue::Unused)
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
                            exploration.new_type_view(operand.ty, &TypesBounds::inactive()),
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

fn ascribe_detached(
    detached: &Detached,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let expr = ascribe_types(
        exploration,
        links,
        diagnostics,
        &detached.underlying,
        state.with_local_value(ExpressionValue::Unused),
    );
    TypedExpr {
        kind: ExprKind::Subprocess(Subprocess {
            inner: Box::new(expr),
            awaited: false,
        }),
        ty: PID,
        segment: detached.segment(),
    }
}

fn ascribe_subshell(
    subshell: &Subshell,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let block = subshell
        .expressions
        .iter()
        .map(|expr| {
            ascribe_types(
                exploration,
                links,
                diagnostics,
                expr,
                state.with_local_value(ExpressionValue::Unused),
            )
        })
        .collect::<Vec<_>>();
    TypedExpr {
        kind: ExprKind::Subprocess(Subprocess {
            inner: Box::new(TypedExpr {
                kind: ExprKind::Block(block),
                ty: UNIT,
                segment: subshell.segment.clone(),
            }),
            awaited: true,
        }),
        ty: EXITCODE,
        segment: subshell.segment(),
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
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedExpr {
    let function_source = links.env().get_raw_env(fun.segment()).unwrap();

    // check if the function declaration is already known
    if exploration.type_engine.get_user(function_source).is_none() {
        // if not, forward declare it by typing its declared signature
        let declaration_link = links.with_source(function_source);
        let forward_declared_chunk =
            declare_function(fun, exploration, declaration_link, diagnostics);
        exploration
            .type_engine
            .insert(function_source, forward_declared_chunk);
    }

    let chunk = exploration.type_engine.get_user(function_source).unwrap();

    let type_ref = TypeRef::new(exploration.externals.current, chunk.function_type);

    let id = links.env().get_raw_symbol(fun.segment()).unwrap();

    let SymbolRef::Local(local_id) = id else {
        unreachable!()
    };

    exploration
        .ctx
        .set_local_typed(links.source, local_id, type_ref);

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
    let left_type = left_expr.ty;
    let right_type = right_expr.ty;
    let name = name_operator_method(bin.op);

    let methods = exploration
        .get_methods(left_expr.ty, name)
        .map(|methods| methods.as_slice())
        .unwrap_or(&[]);

    let method =
        find_operand_implementation(exploration, left_type.reef, methods, left_expr, right_expr);
    match method {
        Ok(method) => TypedExpr {
            ty: method.return_type,
            kind: ExprKind::MethodCall(method.into()),
            segment: bin.segment(),
        },
        Err(left) => {
            diagnostics.push(
                Diagnostic::new(DiagnosticID::UnknownMethod, "Undefined operator")
                    .with_observation(Observation::here(
                        links.source,
                        exploration.externals.current,
                        bin.segment(),
                        format!(
                            "No operator `{}` between type `{}` and `{}`",
                            name,
                            exploration.new_type_view(left_type, &TypesBounds::inactive()),
                            exploration.new_type_view(right_type, &TypesBounds::inactive()),
                        ),
                    )),
            );
            left
        }
    }
}

fn ascribe_subscript(
    sub: &Subscript,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    match create_subscript(sub, exploration, links, diagnostics, state) {
        Ok(method) => TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(method.left),
                arguments: vec![method.right],
                function_id: method.function_id,
            }),
            ty: method.return_type,
            segment: sub.segment(),
        },
        Err(target) => target,
    }
}

fn ascribe_range(
    range: &Iterable,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    match range {
        Iterable::Files(files) => {
            let mut pattern = ascribe_types(
                exploration,
                links,
                diagnostics,
                &files.pattern,
                state.with_local_value(ExpressionValue::Expected(STRING)),
            );
            if pattern.ty == STRING {
                pattern.ty = GLOB;
            } else if pattern.ty.is_ok() {
                panic!("pattern should be of type String");
            }
            pattern
        }
        r => todo!("ascribe range {r:?}"),
    }
}

fn ascribe_tilde(
    tilde: &ast::variable::TildeExpansion,
    exploration: &mut Exploration,
    links: Links,
    diagnostics: &mut Vec<Diagnostic>,
    state: TypingState,
) -> TypedExpr {
    let pfc = ProgrammaticCall {
        path: vec![InclusionPathItem::Symbol("~", tilde.segment())],
        segment: tilde.segment(),
        arguments: match &tilde.structure {
            Tilde::HomeDir(Some(username)) => vec![username.as_ref().clone()],
            Tilde::HomeDir(None) | Tilde::WorkingDir => Vec::new(),
        },
        type_parameters: vec![],
    };
    let typed = ascribe_pfc(&pfc, exploration, links, diagnostics, state);
    generate_unwrap(typed, exploration)
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

    if expr.ty.is_ok()
        && convert_description(exploration, ty, expr.ty, &mut TypesBounds::inactive(), true)
            .is_err()
    {
        diagnostics.push(
            Diagnostic::new(
                DiagnosticID::IncompatibleCast,
                format!(
                    "Casting `{}` as `{}` is invalid",
                    exploration.new_type_view(expr.ty, &TypesBounds::inactive()),
                    exploration.new_type_view(ty, &TypesBounds::inactive()),
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
        state.with_local_value(ExpressionValue::Unspecified),
    );
    if expr.ty.is_err() {
        return expr;
    }

    match unary.op {
        UnaryOperator::Not => ascribe_not(expr, unary.segment(), exploration, links, diagnostics),
        UnaryOperator::Negate => {
            let method = exploration.get_method_exact(expr.ty, "neg", &[], expr.ty);

            match method {
                Some((method, method_id)) => TypedExpr {
                    kind: ExprKind::MethodCall(MethodCall {
                        callee: Box::new(expr),
                        arguments: vec![],
                        function_id: method_id,
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
                                    exploration.new_type_view(expr.ty, &TypesBounds::inactive()),
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
    let (not_method, not_method_id) = lang
        .typed_engine
        .get_method_exact(BOOL_STRUCT, "not", &[], BOOL)
        .expect("A Bool should be invertible");
    match convert_expression(
        not,
        BOOL,
        &mut TypesBounds::inactive(),
        exploration,
        links.source,
        diagnostics,
    ) {
        Ok(expr) => TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(expr),
                arguments: vec![],
                function_id: not_method_id,
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
                            exploration.new_type_view(expr.ty, &TypesBounds::inactive()),
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

    let ty = if state.local_value != ExpressionValue::Unused {
        match convert_many(
            exploration,
            &mut TypesBounds::inactive(),
            [then.ty, otherwise.as_ref().map_or(UNIT, |expr| expr.ty)],
        ) {
            Ok(ty) => {
                // Generate appropriate casts and implicits conversions
                then = convert_expression(
                    then,
                    ty,
                    &mut TypesBounds::inactive(),
                    exploration,
                    links.source,
                    diagnostics,
                )
                .expect("Type mismatch should already have been caught");
                otherwise = otherwise.map(|expr| {
                    convert_expression(
                        expr,
                        ty,
                        &mut TypesBounds::inactive(),
                        exploration,
                        links.source,
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
                    links.source,
                    current_reef,
                    block.success_branch.segment(),
                    format!(
                        "Found `{}`",
                        exploration.new_type_view(then.ty, &TypesBounds::inactive()),
                    ),
                ));
                if let Some(otherwise) = &otherwise {
                    diagnostic = diagnostic.with_observation(Observation::here(
                        links.source,
                        current_reef,
                        otherwise.segment(),
                        format!(
                            "Found `{}`",
                            exploration.new_type_view(otherwise.ty, &TypesBounds::inactive()),
                        ),
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
    if let [Expr::Literal(Literal {
        parsed: LiteralValue::String(cmd),
        segment,
    }), ..] = call.arguments.as_slice()
    {
        if cmd.as_str() == "cd" {
            let pfc = ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(cmd, segment.clone())],
                segment: call.segment(),
                arguments: call.arguments[1..].to_vec(),
                type_parameters: vec![],
            };
            return ascribe_pfc(&pfc, exploration, links, diagnostics, state);
        }
    }

    let args = call
        .arguments
        .iter()
        .map(|expr| {
            let expr = ascribe_types(exploration, links, diagnostics, expr, state);
            if expr.ty == GLOB {
                let glob = exploration
                    .get_method_exact(expr.ty, "spread", &[], builtin::STRING_VEC)
                    .expect("glob method not found");
                let segment = expr.segment.clone();
                TypedExpr {
                    kind: ExprKind::MethodCall(MethodCall {
                        callee: Box::new(expr),
                        arguments: vec![],
                        function_id: glob.1,
                    }),
                    ty: glob.0.return_type,
                    segment,
                }
            } else {
                convert_into_string(expr, exploration, diagnostics, links.source)
            }
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
    let function_match = type_call(call, exploration, links, state, diagnostics);
    TypedExpr {
        kind: ExprKind::FunctionCall(FunctionCall {
            arguments: function_match.arguments,
            function_id: function_match.function_id,
            source_id: function_match.function_source,
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

    let return_hint = if let ExpressionValue::Expected(ty) = state.local_value {
        Some(ty)
    } else {
        None
    };

    match type_method(
        method,
        &callee,
        links,
        arguments,
        diagnostics,
        exploration,
        links.source,
        return_hint,
    ) {
        Some(fun) => TypedExpr {
            kind: ExprKind::MethodCall(MethodCall {
                callee: Box::new(callee),
                arguments: fun.arguments,
                function_id: fun.function_id,
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
                state.with_local_value(ExpressionValue::Unspecified),
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
        state
            .with_in_loop()
            .with_local_value(ExpressionValue::Unused),
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
        Expr::FunctionDeclaration(fd) => {
            ascribe_function_declaration(fd, exploration, links, diagnostics)
        }
        Expr::StructDeclaration(decl) => {
            ascribe_struct_declaration(decl, exploration, links, diagnostics)
        }
        Expr::Literal(lit) => ascribe_literal(lit),
        Expr::TemplateString(tpl) => {
            ascribe_template_string(tpl, exploration, links, diagnostics, state)
        }
        Expr::Assign(assign) => ascribe_assign(assign, exploration, links, diagnostics, state),
        Expr::VarDeclaration(decl) => {
            ascribe_var_declaration(decl, exploration, links, diagnostics, state)
        }
        Expr::VarReference(var) => ascribe_var_reference(var, links, exploration),
        Expr::FieldAccess(fa) => ascribe_field_access(fa, links, exploration, diagnostics, state),
        Expr::Identifier(ident) => ascribe_identifier(ident, links, exploration),
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
        Expr::Detached(detached) => {
            ascribe_detached(detached, exploration, links, diagnostics, state)
        }
        Expr::Subshell(subshell) => {
            ascribe_subshell(subshell, exploration, links, diagnostics, state)
        }
        Expr::Return(r) => ascribe_return(r, exploration, links, diagnostics, state),
        Expr::Parenthesis(paren) => {
            ascribe_types(exploration, links, diagnostics, &paren.expression, state)
        }
        Expr::Unary(unary) => ascribe_unary(unary, exploration, links, diagnostics, state),
        Expr::Binary(bo) => ascribe_binary(bo, exploration, links, diagnostics, state),
        Expr::Subscript(sub) => ascribe_subscript(sub, exploration, links, diagnostics, state),
        Expr::Range(range) => ascribe_range(range, exploration, links, diagnostics, state),
        Expr::Tilde(tilde) => ascribe_tilde(tilde, exploration, links, diagnostics, state),
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

    use crate::analyze;
    use crate::importer::StaticImporter;
    use crate::name::Name;
    use crate::reef::{Reef, ReefId};
    use crate::relations::LocalId;
    use crate::types::engine::{ChunkKind, FunctionId, StructureId};
    use crate::types::hir::{Convert, MethodCall};
    use crate::types::ty::{Type, TypeId};
    use crate::types::{EXITCODE, UNIT};

    use super::*;

    pub(crate) fn extract(source: Source) -> Result<Externals, Vec<Diagnostic>> {
        let name = Name::new(source.name);
        let mut externals = Externals::default();
        let mut importer = StaticImporter::new([(name.clone(), source)], parse_trusted);
        let analyzer = analyze(name, &mut importer, &externals);

        if !analyzer.diagnostics.is_empty() {
            return Err(analyzer.diagnostics);
        }

        externals.register(Reef::new(source.name.to_string(), analyzer));

        Ok(externals)
    }

    pub(crate) fn extract_expr(source: Source) -> Result<Vec<TypedExpr>, Vec<Diagnostic>> {
        extract(source).map(|externals| {
            let chunk = externals
                .get_reef(ReefId(1))
                .unwrap()
                .typed_engine
                .get_user(SourceId(0))
                .unwrap();

            if let ChunkKind::DefinedFunction(Some(body)) = &chunk.kind {
                if let ExprKind::Block(exprs) = &body.kind {
                    return exprs.clone();
                }
            }
            unreachable!()
        })
    }

    pub(crate) fn extract_type(source: Source) -> Result<TypeRef, Vec<Diagnostic>> {
        let externals = extract(source)?;

        let reef = externals.get_reef(ReefId(1)).unwrap();
        let chunk = reef.typed_engine.get_user(SourceId(0)).unwrap();

        if let ChunkKind::DefinedFunction(Some(body)) = &chunk.kind {
            return Ok(body.ty);
        }
        unreachable!()
    }

    #[test]
    fn single_literal() {
        let res = extract_type(Source::unknown("1"));
        assert_eq!(res, Ok(INT));
    }

    #[test]
    fn correct_type_annotation() {
        let externals = extract(Source::unknown("val a: Int = 1")).expect("got errors");
        let test_reef = externals.get_reef(ReefId(1)).unwrap();

        let type_var = test_reef
            .type_context
            .get(
                &test_reef.relations,
                SourceId(0),
                SymbolRef::Local(LocalId(0)),
            )
            .unwrap();

        assert_eq!(type_var.type_ref, INT);
    }

    #[test]
    fn coerce_type_annotation() {
        let externals = extract(Source::unknown("val a: Float = 1")).expect("got errors");
        let test_reef = externals.get_reef(ReefId(1)).unwrap();

        let type_var = test_reef
            .type_context
            .get(
                &test_reef.relations,
                SourceId(0),
                SymbolRef::Local(LocalId(0)),
            )
            .unwrap();

        assert_eq!(type_var.type_ref, FLOAT);
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
        assert_eq!(res, Ok(UNIT));
    }

    #[test]
    fn var_assign_increment() {
        let res = extract_type(Source::unknown("var n = 'Hello, '; n += 'world!'"));
        assert_eq!(res, Ok(UNIT));
    }

    #[test]
    fn invalid_left_hand_side_assignment() {
        let content = "var foo = 1; foo = 'bar' = 9";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::InvalidAssignment,
                "Invalid left-hand side of assignment",
            )
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, "'bar'"),
                "Cannot assign to this expression",
            ))])
        );
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
        assert_eq!(res, Ok(UNIT));
    }

    #[test]
    fn condition_different_type() {
        let res = extract_type(Source::unknown("if false; 4.7; else {}"));
        assert_eq!(res, Ok(UNIT));
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
        assert_eq!(res, Ok(STRING));
    }

    #[test]
    fn function_return_type() {
        let res = extract_type(Source::unknown("fun one() -> Int = 1\none()"));
        assert_eq!(res, Ok(INT));
    }

    #[test]
    fn local_type_only_at_end_of_block() {
        let content = "fun test() -> Int = {if false; 5; else {}; 4}; test()";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(INT));
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
        assert_eq!(res, Ok(INT));
    }

    #[test]
    fn bidirectional_usage() {
        let res = extract_type(Source::unknown(
            "val PI = 3.14\nfun circle(r: Float) -> Float = $(( $PI * $r * $r ))\ncircle(1)",
        ));
        assert_eq!(res, Ok(FLOAT));
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
                "Found `Int`",
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
        assert_eq!(res, Ok(UNIT));
    }

    #[test]
    fn continue_and_break_inside_loops() {
        let content = "loop { continue }; loop { break }";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(UNIT));
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
                    "`continue` must be declared inside a loop",
                )
                .with_observation((SourceId(0), ReefId(1), find_in(content, "continue")).into()),
                Diagnostic::new(
                    DiagnosticID::InvalidBreakOrContinue,
                    "`break` must be declared inside a loop",
                )
                .with_observation((SourceId(0), ReefId(1), find_in(content, "break")).into()),
            ])
        );
    }

    #[test]
    fn explicit_valid_return_mixed() {
        let content = "fun some() -> Int = {\nif true; return 5; 9\n}";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(UNIT));
    }

    #[test]
    fn explicit_invalid_return() {
        let content = "fun some() -> String = {if true; return {}; 9}";
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
                find_in(content, "return {}"),
                "Found `Unit`",
            ))
            .with_observation(Observation::here(
                SourceId(1),
                ReefId(1),
                find_in(content, "9"),
                "Found `Int`",
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
                                function_id: FunctionId(1),
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
                                function_id: FunctionId(29),
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
                                function_id: FunctionId(30),
                            }),
                            ty: STRING,
                            segment: find_in(content, "4.2"),
                        },
                    ]),
                    ty: EXITCODE,
                    segment: find_in(content, "grep $n 4.2"),
                },
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
                "No operator `div` between type `Int` and `String`",
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
                "No operator `sub` between type `String` and `Float`",
            ))]),
        );
    }

    #[test]
    fn valid_operator() {
        let content = "val c = 7.3 - 2.4; $c";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(FLOAT));
    }

    #[test]
    fn valid_operator_explicit_method() {
        let content = "val j = 7.3; val c = $j.sub(2.4); $c";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(FLOAT));
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
                "Method is called here",
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
                "Expected `Float`, found `String`",
            ))
            .with_observation(Observation::context(
                SourceId(0),
                ReefId(1),
                find_in(content, ".sub('a')"),
                "Arguments to this method are incorrect",
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
                "No method `to_string` on type `Unit`",
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
                "Type `Float` cannot be used as a condition",
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
                "No operator `mod` between type `Float` and `Float`",
            ))])
        );
    }

    #[test]
    fn operation_and_test() {
        let content = "val m = 101; val is_even = $m % 2 == 0; $is_even";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(BOOL));
    }

    #[test]
    fn condition_command() {
        let res = extract_type(Source::unknown("if nginx -t { echo 'ok' }"));
        assert_eq!(res, Ok(UNIT));
    }

    #[test]
    fn condition_invert_command() {
        let res = extract_type(Source::unknown("if ! nginx -t { echo 'invalid config' }"));
        assert_eq!(res, Ok(UNIT));
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
                "Cannot invert non-boolean type `String`",
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
                "`Unit` does not implement the `neg` method",
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
                "No operator `mod` between type `String` and `Int`",
            ))])
        );
    }

    #[test]
    fn redirect_to_string() {
        let content = "val file = '/tmp/file'; cat /etc/passwd > $file 2>&1";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(EXITCODE));
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
                "No method `to_string` on type `Unit`",
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
                "Redirection happens here",
            ))])
        );
    }

    #[test]
    fn use_pipeline_return() {
        let res = extract_type(Source::unknown(
            "if echo hello | grep -q test | val m = $(cat test) {}",
        ));
        assert_eq!(res, Ok(UNIT));
    }

    #[test]
    fn use_unit_result() {
        let res = extract_type(Source::unknown(
            "fun foo() = { fun bar() = { return }; bar() }",
        ));
        assert_eq!(res, Ok(UNIT));
    }

    #[test]
    fn correct_concretized_type() {
        let res = extract_type(Source::unknown("'Hello, world'.split(' ').push('!')"));
        assert_eq!(res, Ok(UNIT));
    }

    #[test]
    fn access_method_in_parameter() {
        let res = extract_type(Source::unknown(
            "fun len(bytes: Vec[Int]) -> Int = $bytes.len()",
        ));
        assert_eq!(res, Ok(UNIT));
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
                "Expected `String`, found `Unit`",
            ))
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, ".push({})"),
                "Arguments to this method are incorrect",
            ))])
        );
    }

    #[test]
    fn incorrect_index_type() {
        let content = "''.bytes()['a']";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::UnknownMethod,
                "Cannot index into a value of type `Vec[Int]`",
            )
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, "'a'"),
                "`Vec[Int]` indices are of type `Int`",
            ))])
        );
    }

    #[test]
    fn assign_vec_index() {
        let res = extract_type(Source::unknown(
            "fun mul(v: Vec[Float], x: Float) = {
                var n = 0
                while $n < $v.len() {
                    $v[$n] *= $x
                    $n += 1
                }
            }",
        ));
        assert_eq!(res, Ok(UNIT));
    }

    #[test]
    fn assign_vec_index_incorrect_type() {
        let content = "val v = ''.bytes(); $v[0] = 'a'";
        let res = extract_type(Source::unknown(content));
        assert_eq!(
            res,
            Err(vec![Diagnostic::new(
                DiagnosticID::TypeMismatch,
                "Invalid assignment to `Int`",
            )
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, "'a'"),
                "Found `String`",
            ))
            .with_observation(Observation::context(
                SourceId(0),
                ReefId(1),
                find_in(content, "$v[0]"),
                "Expected due to the type of this binding",
            ))])
        );
    }

    #[test]
    fn incorrect_concretized_type() {
        let content =
            "val lines: Vec[String] = 'Hello, world'.split('\\n'); val first: Int = $lines[0]";
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
                "Expected `Int`",
            ))
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, "$lines[0]"),
                "Found `String`",
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
                "Expected `Vec[Float]`",
            ))
            .with_observation(Observation::here(
                SourceId(0),
                ReefId(1),
                find_in(content, "$lines"),
                "Found `Vec[String]`",
            ))])
        );
    }

    #[test]
    fn vec_in_vec() {
        let content = "fun new_vec[T]() -> Vec[T];
        var v: Vec[Vec[Int]] = new_vec()
        $v = new_vec()
        $v[0] = new_vec()";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(UNIT));
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
                "Expected 0 generic arguments",
            ))])
        );
    }

    #[test]
    fn incorrect_type_parameter_assign() {
        let content = "fun id[T]() -> T; val j: Int = id[String]()";
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
                find_in(content, "id[String]()"),
                "Found `String`"
            ))])
        );
    }

    #[test]
    fn explicit_repeated_type_parameter() {
        let source = Source::unknown("fun i[T, U](a: T, b: T) -> U; i[Int, String](4, 5)");
        let res = extract_type(source);
        assert_eq!(res, Ok(STRING));
    }

    #[test]
    fn fun_with_generic_args() {
        let content = "\
            fun push_into[A, B](v: A, vec: Vec[A], b: B) -> B = {
                val x: A = $v
                $vec.push($x)
                $b
            }
        ";
        let src = Source::unknown(content);
        let externals = extract(src).expect("typing errors");
        let typing = &externals.get_reef(ReefId(1)).unwrap().typing;
        assert_eq!(typing.get_type(TypeId(0)), Some(&Type::Polytype));
        assert_eq!(typing.get_type(TypeId(1)), Some(&Type::Polytype));
    }

    #[test]
    fn fun_call_with_generic_args() {
        let content = r#"\
            fun push_into[A, B](v: A, vec: Vec[A], b: B) -> B = {
                val x: A = $v
                $vec.push($x)
                $b
            }

            val vec = "".split(' ')
            val i = push_into("item", $vec, 4)
        "#;
        let src = Source::unknown(content);
        let externals = extract(src).expect("typing errors");
        let context = &externals.get_reef(ReefId(1)).unwrap().type_context;

        // `A` generic argument
        assert_eq!(
            context.get_local(SourceId(1), LocalId(0)),
            Some(TypedVariable::immutable(TypeRef::new(ReefId(1), TypeId(0))))
        );
        // `B` generic argument
        assert_eq!(
            context.get_local(SourceId(1), LocalId(1)),
            Some(TypedVariable::immutable(TypeRef::new(ReefId(1), TypeId(1))))
        );

        // `v` argument
        assert_eq!(
            context.get_local(SourceId(1), LocalId(2)),
            Some(TypedVariable::immutable(TypeRef::new(ReefId(1), TypeId(0))))
        );

        // `vec` argument (has created a new type instantiation)
        assert_eq!(
            context.get_local(SourceId(1), LocalId(3)),
            Some(TypedVariable::immutable(TypeRef::new(ReefId(1), TypeId(2))))
        );

        // `b` argument
        assert_eq!(
            context.get_local(SourceId(1), LocalId(4)),
            Some(TypedVariable::immutable(TypeRef::new(ReefId(1), TypeId(1))))
        );
    }

    #[test]
    fn fun_generic_args_constraints() {
        let content = "\
            fun foo[A, B](v: A, vec: Vec[A], b: B, c: B) -> B = $b

            val vec = ''.bytes()
            val i: Option[Float] = foo('str_in_int_argument', $vec, '7', $vec)
        ";
        let src = Source::unknown(content);
        let errs = extract(src).expect_err("no typing errors");
        assert_eq!(
            errs,
            vec![
                Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch")
                    .with_observation(Observation::here(
                        SourceId(0),
                        ReefId(1),
                        find_in(content, "$vec"),
                        "Expected `Vec[String]`, found `Vec[Int]`",
                    ))
                    .with_observation(Observation::here(
                        SourceId(1),
                        ReefId(1),
                        find_in(content, "vec: Vec[A]"),
                        "Parameter is declared here",
                    )),
                Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch")
                    .with_observation(Observation::here(
                        SourceId(0),
                        ReefId(1),
                        find_in_nth(content, "$vec", 1),
                        "Expected `String`, found `Vec[Int]`",
                    ))
                    .with_observation(Observation::here(
                        SourceId(1),
                        ReefId(1),
                        find_in(content, "c: B"),
                        "Parameter is declared here",
                    )),
                Diagnostic::new(DiagnosticID::TypeMismatch, "Type mismatch")
                    .with_observation(Observation::here(
                        SourceId(0),
                        ReefId(1),
                        find_in(content, "Option[Float]"),
                        "Expected `Option[Float]`",
                    ))
                    .with_observation(Observation::here(
                        SourceId(0),
                        ReefId(1),
                        find_in(content, "foo('str_in_int_argument', $vec, '7', $vec)"),
                        "Found `String`",
                    )),
            ]
        )
    }

    #[test]
    fn string_glob() {
        let content = "val files = p'systemd-*'; echo $files";
        let res = extract_type(Source::unknown(content));
        assert_eq!(res, Ok(EXITCODE));
    }

    #[test]
    fn background_process() {
        let source = Source::unknown("foo &");
        let res = extract_type(source);
        assert_eq!(res, Ok(PID));
    }

    #[test]
    fn subprocess() {
        let source = Source::unknown("(foo)");
        let res = extract_type(source);
        assert_eq!(res, Ok(EXITCODE));
    }

    #[test]
    fn locals_types() {
        let content = "\
            struct Simple {}
            val a: Simple = Simple()
            val b = Simple()

            val c: Int = 7
            val d = 7
        ";
        let src = Source::unknown(content);
        let externals = extract(src).expect("typing errors");
        let reef = externals.get_reef(ReefId(1)).unwrap();
        let typing = &reef.typing;
        let ctx = &reef.type_context;
        let relations = &reef.relations;

        let assert_local_type = |local_id, type_ref| {
            assert_eq!(
                Some(TypedVariable::immutable(type_ref)),
                ctx.get(relations, SourceId(0), SymbolRef::Local(LocalId(local_id)))
            )
        };

        assert_local_type(0, TypeRef::new(ReefId(1), TypeId(0))); //struct Simple

        assert_local_type(1, TypeRef::new(ReefId(1), TypeId(0))); //val a
        assert_local_type(2, TypeRef::new(ReefId(1), TypeId(0))); //val b (inferred)

        assert_local_type(3, INT); //val c
        assert_local_type(4, INT); //val d (inferred)

        // struct Simple
        assert_eq!(
            Some(&Type::Structure(Some(SourceId(1)), StructureId(0))),
            typing.get_type(TypeId(0))
        );
    }

    #[test]
    fn locals_complex_types() {
        let content = "\
            struct Complex[A] {}
            val a: Complex[Int] = Complex()
            val b = Complex[Int]()
        ";
        let src = Source::unknown(content);
        let externals = extract(src).expect("typing errors");
        let reef = externals.get_reef(ReefId(1)).unwrap();
        let typing = &reef.typing;
        let ctx = &reef.type_context;
        let relations = &reef.relations;

        let assert_local_type = |local_id, type_ref| {
            assert_eq!(
                Some(TypedVariable::immutable(type_ref)),
                ctx.get(relations, SourceId(0), SymbolRef::Local(LocalId(local_id)))
            )
        };

        //struct Complex[A]
        assert_local_type(0, TypeRef::new(ReefId(1), TypeId(0)));

        //val a, references instance of Complex[A] (Complex[Int])
        assert_local_type(1, TypeRef::new(ReefId(1), TypeId(5)));
        //val b (inferred), references equivalent instance of a (Complex[Int])
        assert_local_type(2, TypeRef::new(ReefId(1), TypeId(6)));

        // struct Complex[A]
        assert_eq!(
            Some(&Type::Structure(Some(SourceId(1)), StructureId(0))),
            typing.get_type(TypeId(0))
        );

        // instance 1 Complex[Int]
        assert_eq!(
            Some(&Type::Instantiated(
                TypeRef::new(ReefId(1), TypeId(0)),
                vec![INT]
            )),
            typing.get_type(TypeId(5))
        );

        // instance 2 Complex[Int]
        assert_eq!(
            Some(&Type::Instantiated(
                TypeRef::new(ReefId(1), TypeId(0)),
                vec![INT]
            )),
            typing.get_type(TypeId(6))
        );
    }
}
