use analyzer::types::hir::{ExprKind, TypedExpr};
use analyzer::types::*;

use crate::bytecode::{Bytecode, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit;
use crate::emit::EmissionState;

pub fn emit_process_call(
    arguments: &Vec<TypedExpr>,
    use_return: bool,
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    state.literal_strings = true;
    for arg in arguments {
        emit(arg, emitter, cp, state);
        if matches!(arg.kind, ExprKind::Literal(_)) {
            // literals are already converted thanks to state.literal_strings set to true
            continue;
        }
        match arg.ty {
            INT => emitter.emit_code(Opcode::ConvertIntToStr),
            FLOAT => emitter.emit_code(Opcode::ConvertFloatToStr),
            STRING => {}
            _ => todo!("Convert to other types"),
        }
    }
    state.literal_strings = false;

    emitter.emit_code(Opcode::Spawn);
    emitter.bytes.push(arguments.len() as u8);

    if !use_return {
        // The Spawn operation will push the process's exitcode onto the stack
        // in order to maintain the stack's size, we instantly pop
        // the stack if the value isn't used later in the code
        emitter.emit_code(Opcode::PopByte)
    }
}
