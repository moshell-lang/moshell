use crate::bytecode::{Instructions, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState};
use analyzer::engine::Engine;
use analyzer::relations::NativeId;
use analyzer::types::hir::TypedExpr;
use analyzer::types::Typing;

/// Emits a primitive sequence of instructions.
#[allow(clippy::too_many_arguments)]
pub(crate) fn emit_primitive_op(
    native: NativeId,
    callee: &TypedExpr,
    args: &[TypedExpr],
    emitter: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    emit(callee, emitter, typing, engine, cp, state);
    match native.0 {
        0 => {
            // ExitCode -> Bool
            emitter.emit_push_byte(1);
            emitter.emit_byte_binary_op(Opcode::BXor);
        }
        1..=9 => {
            // Arithmetic expression
            emit(
                args.get(0).expect("A binary expression takes two operands"),
                emitter,
                typing,
                engine,
                cp,
                state,
            );
            emitter.emit_q_word_binary_op(match native.0 {
                1 => Opcode::IntAdd,
                3 => Opcode::IntSub,
                5 => Opcode::IntMul,
                7 => Opcode::IntDiv,
                9 => Opcode::IntMod,
                _ => todo!("Binary expression with float and byte types"),
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
        17 => {
            // String + String -> String
            emit(
                args.get(0)
                    .expect("Cannot concatenate a string without a second string"),
                emitter,
                typing,
                engine,
                cp,
                state,
            );
            emitter.emit_q_word_binary_op(Opcode::Concat);
        }
        id => todo!("Native function with id {id}"),
    }
}
