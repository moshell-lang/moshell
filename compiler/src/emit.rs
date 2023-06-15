use analyzer::relations::Symbol;
use analyzer::types::*;
use analyzer::types::hir::{Declaration, ExprKind, TypedExpr};
use ast::value::LiteralValue;

use crate::bytecode::{Bytecode, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::invoke::emit_process_call;
use crate::emit::jump::{emit_break, emit_conditional, emit_continue, emit_loop};

mod invoke;
mod jump;


pub struct EmissionState {
    // the start instruction position of the enclosing loop
    // set to 0 if there is no loop
    pub last_loop_start: usize,
    // All the placeholders waiting for the end of the loop.
    // When the loop compilation ends, all those placeholder are filled with the
    // first instruction pointer after the loop.
    pub last_loop_end_placeholders: Vec<usize>,

    // if set to true, the compiler will try
    // to convert literals to string at compile time
    // instead of converting to string them via bytecode opcodes (at runtime)
    pub literal_strings: bool
}


impl EmissionState {
    pub fn new() -> Self {
        Self {
            last_loop_start: 0,
            last_loop_end_placeholders: Vec::new(),
            literal_strings: false
        }
    }
}

fn emit_literal(literal: &LiteralValue,
                emitter: &mut Bytecode,
                cp: &mut ConstantPool,
                state: &mut EmissionState) {
    match literal {
        LiteralValue::String(string) => {
            let str_ref = cp.insert_string(string.clone());
            emitter.emit_string_constant_ref(str_ref);
        }
        LiteralValue::Int(integer) => {
            if state.literal_strings {
                let str_ref = cp.insert_string(integer.to_string());
                emitter.emit_string_constant_ref(str_ref);
                return;
            }
            emitter.emit_int(*integer);
        }
        LiteralValue::Float(f) => {
            if state.literal_strings {
                let str_ref = cp.insert_string(f.to_string());
                emitter.emit_string_constant_ref(str_ref);
                return;
            }
            emitter.emit_float(*f);
        }
    }
}

fn emit_ref(symbol: &Symbol, emitter: &mut Bytecode) {
    match symbol {
        Symbol::Local(id) => {
            emitter.emit_get_local(id.0 as u8)
        }
        _ => todo!(),
    }
}

fn emit_declaration(declaration: &Declaration,
                    emitter: &mut Bytecode,
                    cp: &mut ConstantPool,
                    state: &mut EmissionState) {
    let value = declaration.value.as_ref().expect("var declaration without initializer not supported");
    emit(value, emitter, cp, state);
    emitter.emit_code(Opcode::SetLocal);
    emitter.bytes.push(declaration.identifier.0 as u8);
}

fn emit_block(exprs: &Vec<TypedExpr>,
              emitter: &mut Bytecode,
              cp: &mut ConstantPool,
              state: &mut EmissionState) {
    for expr in exprs {
        emit(expr, emitter, cp, state);
    }
}

pub fn emit(expr: &TypedExpr,
            emitter: &mut Bytecode,
            cp: &mut ConstantPool,
            state: &mut EmissionState) {
    match &expr.kind {
        ExprKind::Declare(d) => emit_declaration(d, emitter, cp, state),
        ExprKind::Reference(r) => emit_ref(r, emitter),
        ExprKind::Literal(literal) => emit_literal(literal, emitter, cp, state),
        ExprKind::ProcessCall(args) => emit_process_call(args, expr.ty != NOTHING, emitter, cp, state),
        ExprKind::Block(exprs) => emit_block(exprs, emitter, cp, state),
        ExprKind::Conditional(c) => emit_conditional(c, emitter, cp, state),
        ExprKind::ConditionalLoop(l) => emit_loop(l, emitter, cp, state),
        ExprKind::Continue => emit_continue(emitter, state),
        ExprKind::Break => emit_break(emitter, state),
        _ => {}
    }
}
