use analyzer::relations::Symbol;
use analyzer::types::*;
use analyzer::types::hir::{Declaration, ExprKind, TypedExpr};
use ast::value::LiteralValue;

use crate::bytecode::{Instructions, Opcode, Placeholder};
use crate::constant_pool::ConstantPool;
use crate::emit::invoke::{emit_function_call, emit_process_call};
use crate::emit::jump::{emit_break, emit_conditional, emit_continue, emit_loop};

mod invoke;
mod jump;

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
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    if let Some(value) = &declaration.value {
        emit(value, emitter, typing, cp, state);
        emitter.emit_code(Opcode::SetLocal);
        emitter.bytecode.emit_byte(declaration.identifier.0 as u8);
    }
}

fn emit_block(
    exprs: &Vec<TypedExpr>,
    emitter: &mut Instructions,
    typing: &Typing,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    for expr in exprs {
        emit(expr, emitter, typing, cp, state);
    }
}


pub fn emit(
    expr: &TypedExpr,
    instructions: &mut Instructions,
    typing: &Typing,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    let use_return = expr.ty != NOTHING;
    match &expr.kind {
        ExprKind::Declare(d) => emit_declaration(d, instructions, typing, cp, state),
        ExprKind::Block(exprs) => emit_block(exprs, instructions, typing, cp, state),
        ExprKind::Conditional(c) => emit_conditional(c, instructions, typing, cp, state),
        ExprKind::ConditionalLoop(l) => emit_loop(l, instructions, typing, cp, state),
        ExprKind::Continue => emit_continue(instructions, state),
        ExprKind::Break => emit_break(instructions, state),
        ExprKind::Reference(r) => if use_return {
            emit_ref(r, instructions)
        }
        ExprKind::Literal(literal) => if use_return {
            emit_literal(literal, instructions, cp)
        }
        ExprKind::ProcessCall(args) => emit_process_call(args, use_return, instructions, typing, cp, state),
        ExprKind::FunctionCall(fc) => emit_function_call(fc, expr.ty, instructions, typing, cp, state),
        _ => unimplemented!()
    }
}
