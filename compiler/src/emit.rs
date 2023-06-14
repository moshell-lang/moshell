use analyzer::relations::Symbol;
use analyzer::types::hir::{Declaration, ExprKind, TypedExpr};
use ast::value::LiteralValue;

use crate::bytecode::{Bytecode, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::invoke::emit_process_call;
use crate::emit::jump::emit_conditional;

pub mod invoke;
pub mod jump;

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
        Symbol::Local(id) => {
            emitter.emit_get_local(id.0 as u8)
        }
        _ => todo!(),
    }
}

fn emit_declaration(declaration: &Declaration, emitter: &mut Bytecode, cp: &mut ConstantPool) {
    let value = declaration.value.as_ref().expect("var declaration without initializer not supported");
    emit(value, emitter, cp);
    emitter.emit_code(Opcode::SetLocal);
    emitter.bytes.push(declaration.identifier.0 as u8);
}

fn emit_block(exprs: &Vec<TypedExpr>, emitter: &mut Bytecode, cp: &mut ConstantPool) {
    for expr in exprs {
        emit(expr, emitter, cp);
    }
}

pub fn emit(expr: &TypedExpr, emitter: &mut Bytecode, cp: &mut ConstantPool) {
    match &expr.kind {
        ExprKind::Declare(d) => emit_declaration(d, emitter, cp),
        ExprKind::Reference(r) => emit_ref(r, emitter),
        ExprKind::Literal(literal) => emit_literal(literal, emitter, cp),
        ExprKind::ProcessCall(args) => emit_process_call(args, emitter, cp),
        ExprKind::Block(exprs) => emit_block(exprs, emitter, cp),
        ExprKind::Conditional(c) => emit_conditional(c, emitter, cp),
        _ => {}
    }
}
