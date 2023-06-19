use crate::bytecode::{Bytecode, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState};
use analyzer::relations::NativeId;
use analyzer::types::hir::TypedExpr;

/// Emits a native sequence of instructions.
pub(crate) fn emit_native(
    native: NativeId,
    callee: &TypedExpr,
    args: &[TypedExpr],
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    emit(&callee, emitter, cp, state);
    match native.0 {
        0 => {
            // ExitCode -> Bool
            emitter.emit_byte(1);
            emitter.emit_code(Opcode::BXor);
        }
        1..=9 => {
            // Arithmetic expression
            emit(
                args.get(0).expect("A binary expression takes two operands"),
                emitter,
                cp,
                state,
            );
            emitter.emit_code(match native.0 {
                1 => Opcode::IntAdd,
                3 => Opcode::IntSub,
                5 => Opcode::IntMul,
                7 => Opcode::IntDiv,
                9 => Opcode::IntMod,
                _ => todo!("Binary expression with float type"),
            });
        }
        13 => {
            // Int -> String
            emitter.emit_code(Opcode::ConvertIntToStr);
        }
        14 => {
            // Float -> String
            emitter.emit_code(Opcode::ConvertFloatToStr);
        }
        id => todo!("Native function with id {id}"),
    }
}
