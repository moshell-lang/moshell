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

    //set this to false if the emitted code does not pushes a q_word value.
    let mut q_word_pushed = true;

    match native.0 {
        0 => {
            // ExitCode -> Bool
            emitter.emit_bool_inversion();
            q_word_pushed = false
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
        }
        10 => {
            // !bool
            emitter.emit_bool_inversion();
            q_word_pushed = false
        }
        11..=26 => {
            // Comparison expression
            emit(
                args.get(0).expect("A comparison takes two operands"),
                emitter,
                typing,
                engine,
                cp,
                state,
            );

            match native.0 {
                11 => {
                    // bool == bool
                    emitter.emit_code(Opcode::ByteEqual);
                    q_word_pushed = false;
                }
                12 => {
                    // bool != bool
                    emitter.emit_code(Opcode::ByteEqual);
                    emitter.emit_bool_inversion();
                    q_word_pushed = false;
                }
                13 => {
                    // String == String
                    emitter.emit_code(Opcode::StringEqual);
                }
                14 => {
                    // String != String
                    emitter.emit_code(Opcode::StringEqual);
                    emitter.emit_bool_inversion();
                }
                15 => emitter.emit_code(Opcode::IntEqual),
                16 => {
                    // Int != Int
                    emitter.emit_code(Opcode::IntEqual);
                    emitter.emit_bool_inversion();
                }
                17 => emitter.emit_code(Opcode::IntLessThan),
                18 => emitter.emit_code(Opcode::IntLessOrEqual),
                19 => emitter.emit_code(Opcode::IntGreaterThan),
                20 => emitter.emit_code(Opcode::IntGreaterOrEqual),
                21 => emitter.emit_code(Opcode::FloatEqual),
                22 => {
                    // Float != Float
                    emitter.emit_code(Opcode::FloatEqual);
                    emitter.emit_bool_inversion();
                }
                23 => emitter.emit_code(Opcode::FloatLessThan),
                24 => emitter.emit_code(Opcode::FloatLessOrEqual),
                25 => emitter.emit_code(Opcode::FloatGreaterThan),
                26 => emitter.emit_code(Opcode::FloatGreaterOrEqual),
                _ => unreachable!("Not a comparison expression"),
            }
        }
        27 => {
            // Bool -> String
            // Emit the opcodes for:
            // if (bool) {
            //     "true"
            // } else {
            //     "false"
            // }
            let true_string = cp.insert_string("true");
            let false_string = cp.insert_string("false");
            let jump_to_else = emitter.emit_jump(Opcode::IfNotJump);
            emitter.bytecode.emit_constant_ref(true_string);
            let jump_to_end = emitter.emit_jump(Opcode::Jump);
            emitter.patch_jump(jump_to_else);
            emitter.bytecode.emit_constant_ref(false_string);
            emitter.patch_jump(jump_to_end);
        }
        28 => {
            // ExitCode -> String
            emitter.emit_code(Opcode::ConvertByteToInt);
            emitter.emit_code(Opcode::ConvertIntToStr);
        }
        29 => {
            // Int -> String
            emitter.emit_code(Opcode::ConvertIntToStr);
        }
        30 => {
            // Float -> String
            emitter.emit_code(Opcode::ConvertFloatToStr);
        }
        33 => {
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
        }
        id => todo!("Native function with id {id}"),
    };

    if !state.use_values {
        if q_word_pushed {
            emitter.emit_pop_q_word()
        } else {
            emitter.emit_pop_byte()
        }
    }
}
