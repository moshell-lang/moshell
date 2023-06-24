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
    let last = state.use_values(true);
    emit(callee, emitter, typing, engine, cp, state);
    state.use_values(last);

    let pop_qword = match native.0 {
        0 => {
            // ExitCode -> Bool
            emitter.emit_push_byte(1);
            emitter.emit_code(Opcode::BXor);
            false
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
            emitter.emit_code(match native.0 {
                1 => Opcode::IntAdd,
                3 => Opcode::IntSub,
                5 => Opcode::IntMul,
                7 => Opcode::IntDiv,
                9 => Opcode::IntMod,
                _ => todo!("Binary expression with float and byte types"),
            });
            true
        }
        13 => {
            // Int -> String
            emitter.emit_code(Opcode::ConvertIntToStr);
            true
        }
        14 => {
            // Float -> String
            emitter.emit_code(Opcode::ConvertFloatToStr);
            true
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
            emitter.emit_code(Opcode::Concat);
            true
        }
        id => todo!("Native function with id {id}"),
    };

    if !state.use_values {
        if pop_qword {
            emitter.emit_pop_q_word()
        } else {
            emitter.emit_pop_byte()
        }
    }
}
