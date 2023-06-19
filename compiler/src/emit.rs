use analyzer::relations::{Definition, Symbol};
use analyzer::types::hir::{Declaration, ExprKind, TypedExpr};
use analyzer::types::*;
use ast::value::LiteralValue;

use crate::bytecode::{Bytecode, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::invoke::emit_process_call;
use crate::emit::jump::{emit_break, emit_conditional, emit_continue, emit_loop};
use crate::emit::native::emit_native;

mod invoke;
mod jump;
mod native;

#[derive(Debug, Clone, Default)]
pub struct EmissionState {
    // the start instruction position of the enclosing loop
    // set to 0 if there is no loop
    pub enclosing_loop_start: usize,
    // All the placeholders waiting for the end of the loop.
    // When the loop compilation ends, all those placeholder are filled with the
    // first instruction pointer after the loop.
    pub enclosing_loop_end_placeholders: Vec<usize>,
}

impl EmissionState {
    /// Create a new emission state.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new emission state for a loop.
    pub fn in_loop(loop_start: usize) -> Self {
        Self {
            enclosing_loop_start: loop_start,
            enclosing_loop_end_placeholders: Vec::new(),
        }
    }
}

fn emit_literal(literal: &LiteralValue, emitter: &mut Bytecode, cp: &mut ConstantPool) {
    match literal {
        LiteralValue::String(string) => {
            let str_ref = cp.insert_string(string.clone());
            emitter.emit_string_constant_ref(str_ref);
        }
        LiteralValue::Int(integer) => {
            emitter.emit_int(*integer);
        }
        LiteralValue::Float(f) => {
            emitter.emit_float(*f);
        }
    }
}

fn emit_ref(symbol: &Symbol, emitter: &mut Bytecode) {
    match symbol {
        Symbol::Local(id) => emitter.emit_get_local(id.0 as u8),
        _ => todo!(),
    }
}

fn emit_declaration(
    declaration: &Declaration,
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    if let Some(value) = &declaration.value {
        emit(value, emitter, cp, state);
        emitter.emit_code(Opcode::SetLocal);
        emitter.bytes.push(declaration.identifier.0 as u8);
    }
}

fn emit_block(
    exprs: &Vec<TypedExpr>,
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    for expr in exprs {
        emit(expr, emitter, cp, state);
    }
}

pub fn emit(
    expr: &TypedExpr,
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    let use_return = expr.ty != NOTHING;
    match &expr.kind {
        ExprKind::Declare(d) => emit_declaration(d, emitter, cp, state),
        ExprKind::Block(exprs) => emit_block(exprs, emitter, cp, state),
        ExprKind::Conditional(c) => emit_conditional(c, emitter, cp, state),
        ExprKind::ConditionalLoop(l) => emit_loop(l, emitter, cp, state),
        ExprKind::Continue => emit_continue(emitter, state),
        ExprKind::Break => emit_break(emitter, state),
        ExprKind::Reference(r) => {
            if use_return {
                emit_ref(r, emitter)
            }
        }
        ExprKind::Literal(literal) => {
            if use_return {
                emit_literal(literal, emitter, cp)
            }
        }
        ExprKind::ProcessCall(args) => emit_process_call(args, use_return, emitter, cp, state),
        ExprKind::MethodCall(method) => match method.definition {
            Definition::Native(id) => {
                emit_native(id, &method.callee, &method.arguments, emitter, cp, state)
            }
            _ => todo!("user defined method"),
        },
        _ => {}
    }
}
