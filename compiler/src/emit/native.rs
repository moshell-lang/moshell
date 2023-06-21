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

    let pop_opcode = match native.0 {
        0 => {
            // ExitCode -> Bool
            emitter.invert_bool();
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
                2 => Opcode::FloatAdd,
                3 => Opcode::IntSub,
                4 => Opcode::FloatSub,
                5 => Opcode::IntMul,
                6 => Opcode::FloatMul,
                7 => Opcode::IntDiv,
                8 => Opcode::FloatDiv,
                9 => Opcode::IntMod,
                _ => unreachable!("Not a numeric binary expression"),
            });
            Opcode::PopQWord
        }
        10..=23 => {
            // Comparison expression
            emit(
                args.get(0).expect("A comparison takes two operands"),
                emitter,
                cp,
                state,
            );
            match native.0 {
                10 => { /* Bool == true */ }
                11 => {
                    // Bool == false
                    emitter.invert_bool()
                }
                12 => {
                    // Int == Int
                    emitter.emit_code(Opcode::IntEqual)
                }
                13 => {
                    // Int != Int
                    emitter.emit_code(Opcode::IntEqual);
                    emitter.invert_bool();
                }
                14 => emitter.emit_code(Opcode::IntLessThan),
                15 => emitter.emit_code(Opcode::IntLessOrEqual),
                16 => emitter.emit_code(Opcode::IntGreaterThan),
                17 => emitter.emit_code(Opcode::IntGreaterOrEqual),
                _ => todo!("Comparison expression"),
            }
            Opcode::PopByte
        }
        24 => {
            // Bool -> String
            // Emit the opcodes for:
            // if (bool) {
            //     "true"
            // } else {
            //     "false"
            // }
            let true_string = cp.insert_string("true".to_owned());
            let false_string = cp.insert_string("false".to_owned());
            let jump_to_else = emitter.emit_jump(Opcode::IfNotJump);
            emitter.emit_string_constant_ref(true_string);
            let jump_to_end = emitter.emit_jump(Opcode::Jump);
            emitter.patch_jump(jump_to_else);
            emitter.emit_string_constant_ref(false_string);
            emitter.patch_jump(jump_to_end);
            Opcode::PopQWord
        }
        25 => {
            // ExitCode -> String
            emitter.emit_code(Opcode::ConvertByteToInt);
            emitter.emit_code(Opcode::ConvertIntToStr);
            Opcode::PopQWord
        }
        26 => {
            // Int -> String
            emitter.emit_code(Opcode::ConvertIntToStr);
            Opcode::PopQWord
        }
        27 => {
            // Float -> String
            emitter.emit_code(Opcode::ConvertFloatToStr);
            Opcode::PopQWord
        }
        30 => {
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

    if !state.use_values {
        emitter.emit_code(pop_opcode)
    }
}
