use analyzer::relations::Symbol;
use analyzer::types::hir::{Declaration, ExprKind, TypedExpr};
use ast::value::LiteralValue;

use crate::bytecode::{Bytecode, Opcode};
use crate::emit::invoke::emit_process_call;
use crate::emit::jump::emit_conditional;

pub mod invoke;
pub mod jump;

fn emit_literal(literal: &LiteralValue, emitter: &mut Bytecode) {
    match literal {
        LiteralValue::String(string) => {
            emitter.emit_string_constant(string.to_owned());
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
    emitter.emit_code(Opcode::GetLocal);
    match symbol {
        Symbol::Local(id) => {
            emitter.bytes.push(id.0 as u8);
        }
        _ => todo!(),
    }
}

fn emit_declaration(declaration: &Declaration, emitter: &mut Bytecode) {
    let value = declaration.value.as_ref().expect("var declaration without initializer not supported");
    emit(emitter, value);
    emitter.emit_code(Opcode::SetLocal);
    emitter.bytes.push(declaration.identifier.0 as u8);
}

fn emit_block(exprs: &Vec<TypedExpr>, emitter: &mut Bytecode) {
    for expr in exprs {
        emit(emitter, expr);
    }
}

pub fn emit(emitter: &mut Bytecode, expr: &TypedExpr) {
    match &expr.kind {
        ExprKind::Declare(d) => emit_declaration(d, emitter),
        ExprKind::Reference(r) => emit_ref(r, emitter),
        ExprKind::Literal(literal) => emit_literal(literal, emitter),
        ExprKind::ProcessCall(args) => emit_process_call(args, emitter),
        ExprKind::Block(exprs) => emit_block(exprs, emitter),
        ExprKind::Conditional(c) => emit_conditional(c, emitter),
        _ => {}
    }
}
