use crate::bytecode::{Bytecode, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState};
use analyzer::relations::NativeId;
use analyzer::types::hir::TypedExpr;

/// Emits a primitive sequence of instructions.
pub(crate) fn emit_primitive_op(
    native: NativeId,
    callee: &TypedExpr,
    args: &[TypedExpr],
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    let last = state.use_values(true);
    emit(callee, emitter, cp, state);
    state.use_values(last);

    let pop_opcode= match native.0 {
        0 => {
            // ExitCode -> Bool
            emitter.emit_byte(1);
            emitter.emit_code(Opcode::BXor);
            Opcode::PopByte
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
            Opcode::PopQWord
        }
        13 => {
            // Int -> String
            emitter.emit_code(Opcode::ConvertIntToStr);
            Opcode::PopQWord
        }
        14 => {
            // Float -> String
            emitter.emit_code(Opcode::ConvertFloatToStr);
            Opcode::PopQWord
        }
        17 => {
            // String + String -> String
            emit(
                args.get(0)
                    .expect("Cannot concatenate a string without a second string"),
                emitter,
                cp,
                state,
            );
            emitter.emit_code(Opcode::Concat);
            Opcode::PopQWord
        }
        id => todo!("Native function with id {id}"),
    };

    if state.use_values {
        emitter.emit_code(pop_opcode)
    }
}
