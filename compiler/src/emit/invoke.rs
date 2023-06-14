use analyzer::types::*;
use analyzer::types::hir::TypedExpr;

use crate::bytecode::{Bytecode, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit;

pub fn emit_process_call(arguments: &Vec<TypedExpr>, emitter: &mut Bytecode, cp: &mut ConstantPool) {

    for arg in arguments {
        emit(arg, emitter, cp);
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