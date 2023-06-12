pub mod bytecode;

use crate::bytecode::{Bytecode, Opcode};
use analyzer::relations::Symbol;
use analyzer::types::hir::{ExprKind, TypedExpr};
use analyzer::types::*;
use ast::value::LiteralValue;
use std::io;
use std::io::Write;

pub fn emit(emitter: &mut Bytecode, expr: &TypedExpr) {
    match &expr.kind {
        ExprKind::Declare { identifier, value } => {
            if let Some(value) = value {
                emit(emitter, value);
                emitter.emit_code(Opcode::SetLocal);
                match identifier {
                    Symbol::Local(id) => {
                        emitter.bytes.push(*id as u8);
                    }
                    _ => unreachable!(),
                }
            }
        }
        ExprKind::Reference(symbol) => {
            emitter.emit_code(Opcode::GetLocal);
            match symbol {
                Symbol::Local(id) => {
                    emitter.bytes.push(*id as u8);
                }
                _ => todo!(),
            }
        }
        ExprKind::Literal(literal) => match literal {
            LiteralValue::String(string) => {
                emitter.emit_string_constant(string.to_owned());
            }
            LiteralValue::Int(integer) => {
                emitter.emit_int(*integer);
            }
            LiteralValue::Float(f) => {
                emitter.emit_float(*f);
            }
        },
        ExprKind::ProcessCall(arguments) => {
            for arg in arguments {
                emit(emitter, arg);
                match arg.ty {
                    INT => emitter.emit_code(Opcode::ConvertIntToStr),
                    FLOAT => emitter.emit_code(Opcode::ConvertFloatToStr),
                    STRING => {}
                    _ => todo!("Convert to other types"),
                }
            }
            emitter.emit_code(Opcode::Spawn);
            emitter.bytes.push(arguments.len() as u8);
        }
        ExprKind::Block(block) => {
            for expr in block {
                emit(emitter, expr);
            }
        }
        _ => {}
    }
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
