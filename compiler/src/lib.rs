pub mod bytecode;

use crate::bytecode::{Bytecode, Opcode};
use analyzer::types::hir::{ExprKind, TypedExpr};
use ast::value::LiteralValue;
use std::io;
use std::io::Write;
use analyzer::relations::Symbol;

pub fn emit(emitter: &mut Bytecode, expr: &TypedExpr) -> usize {
    match &expr.kind {
        ExprKind::Declare { identifier, value } => {
            emitter.emit_code(Opcode::SetLocal);
            match identifier {
                Symbol::Local(id) => {
                    emitter.bytes.push(*id as u8);
                }
                _ => {}
            }
            return value.as_ref().map(|value| emit(emitter, &value)).unwrap_or_default() + 1;
        },
        ExprKind::Reference(symbol) => {
            emitter.emit_code(Opcode::GetLocal);
            match symbol {
                Symbol::Local(id) => {
                    emitter.bytes.push(*id as u8);
                }
                _ => {}
            }
        }
        ExprKind::Literal(literal) => match literal {
            LiteralValue::String(string) => {
                emitter.emit_string_constant(string.to_owned());
            }
            LiteralValue::Int(integer) => {
                emitter.emit_int_constant(*integer);
            }
            _ => {}
        },
        ExprKind::ProcessCall(arguments) => {
            for arg in arguments {
                emit(emitter, arg);
            }
            emitter.emit_code(Opcode::Spawn);
            emitter.bytes.push(arguments.len() as u8);
        }
        ExprKind::Block(block) => {
            let mut vars_declarations = 0;
            for expr in block {
                vars_declarations += emit(emitter, expr);
            }
            for _ in 0..vars_declarations {
                emitter.emit_code(Opcode::Pop);
            }
        }
        _ => {}
    }
    0
}

pub fn write(mut writer: impl Write, emitter: Bytecode) -> Result<(), io::Error> {
    writer.write(&[emitter.constants.len() as u8])?;
    for constant in emitter.constants {
        writer.write(&constant.len().to_be_bytes())?;
        writer.write(constant.as_bytes())?;
    }
    writer.write(&emitter.bytes)?;
    Ok(())
}
