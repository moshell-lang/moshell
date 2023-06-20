use analyzer::engine::Engine;
use analyzer::relations::{Definition, Symbol};

use analyzer::types::hir::{Declaration, ExprKind, TypedExpr};
use analyzer::types::*;
use ast::value::LiteralValue;

use crate::bytecode::{Instructions, Opcode, Placeholder};
use crate::constant_pool::ConstantPool;
use crate::emit::invoke::{emit_function_call, emit_process_call};
use crate::emit::jump::{emit_break, emit_conditional, emit_continue, emit_loop};
use crate::emit::native::emit_primitive_op;

mod invoke;
mod jump;
mod native;

#[derive(Debug, Clone, Default)]
pub struct EmissionState {
    // the start instruction position of the enclosing loop
    // set to 0 if there is no loop
    pub enclosing_loop_start: u32,
    // All the placeholders waiting for the end of the loop.
    // When the loop compilation ends, all those placeholder are filled with the
    // first instruction pointer after the loop.
    pub enclosing_loop_end_placeholders: Vec<Placeholder>,
}

impl EmissionState {
    /// Create a new emission state for a loop.
    pub fn in_loop(loop_start: u32) -> Self {
        Self {
            enclosing_loop_start: loop_start,
            enclosing_loop_end_placeholders: Vec::new(),
        }
    }
}

fn emit_literal(literal: &LiteralValue, emitter: &mut Instructions, cp: &mut ConstantPool) {
    match literal {
        LiteralValue::String(string) => {
            let str_ref = cp.insert_string(string);
            emitter.emit_push_string_constant(str_ref);
        }
        LiteralValue::Int(integer) => {
            emitter.emit_push_int(*integer);
        }
        LiteralValue::Float(f) => {
            emitter.emit_push_float(*f);
        }
    }
}

fn emit_ref(symbol: &Symbol, emitter: &mut Instructions) {
    match symbol {
        Symbol::Local(id) => emitter.emit_get_local(id.0 as u8),
        _ => todo!(),
    }
}

fn emit_declaration(
    declaration: &Declaration,
    emitter: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    if let Some(value) = &declaration.value {
        emit(value, emitter, typing, engine, cp, state);
        emitter.emit_code(Opcode::SetLocal);
        emitter.bytecode.emit_byte(declaration.identifier.0 as u8);
    }
}

fn emit_block(
    exprs: &Vec<TypedExpr>,
    emitter: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    for expr in exprs {
        emit(expr, emitter, typing, engine, cp, state);
    }
}

pub fn emit(
    expr: &TypedExpr,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    let use_return = expr.ty != NOTHING;
    match &expr.kind {
        ExprKind::Declare(d) => emit_declaration(d, instructions, typing, engine, cp, state),
        ExprKind::Block(exprs) => emit_block(exprs, instructions, typing, engine, cp, state),
        ExprKind::Conditional(c) => emit_conditional(c, instructions, typing, engine, cp, state),
        ExprKind::ConditionalLoop(l) => emit_loop(l, instructions, typing, engine, cp, state),
        ExprKind::Continue => emit_continue(instructions, state),
        ExprKind::Break => emit_break(instructions, state),
        ExprKind::Reference(r) => {
            if use_return {
                emit_ref(r, instructions)
            }
        }
        ExprKind::Literal(literal) => {
            if use_return {
                emit_literal(literal, instructions, cp)
            }
        }
        ExprKind::FunctionCall(fc) => {
            emit_function_call(fc, expr.ty, instructions, typing, engine, cp, state)
        }
        ExprKind::ProcessCall(args) => {
            emit_process_call(args, use_return, instructions, typing, engine, cp, state)
        }
        ExprKind::MethodCall(method) => match method.definition {
            Definition::Native(id) => {
                emit_primitive_op(
                    id,
                    &method.callee,
                    &method.arguments,
                    instructions,
                    typing,
                    engine,
                    cp,
                    state,
                );
            }
            Definition::User(_) => todo!("invocation of user defined methods"),
        },
        _ => unimplemented!(),
    }
}
