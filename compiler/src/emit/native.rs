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
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    let last = state.use_values(true);
    emit(callee, instructions, typing, engine, cp, state);
    state.use_values(last);

    // The opcode to emit if state.use_value is set to false.
    // defaults to PopQWord but you may want to set it to other variants
    // if the resulting value is anything but an 8 byte value
    let mut pop_opcode = Opcode::PopQWord;

    match native.0 {
        0 => {
            // ExitCode -> Bool
            instructions.emit_bool_inversion();
            pop_opcode = Opcode::PopByte;
        }
        1..=9 => {
            // Arithmetic expression
            emit(
                args.get(0).expect("A binary expression takes two operands"),
                instructions,
                typing,
                engine,
                cp,
                state,
            );
            instructions.emit_code(match native.0 {
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
            instructions.emit_bool_inversion();
            pop_opcode = Opcode::PopByte;
        }
        11..=26 => {
            // Comparison expression
            emit(
                args.get(0).expect("A comparison takes two operands"),
                instructions,
                typing,
                engine,
                cp,
                state,
            );

            match native.0 {
                11 => {
                    // bool == bool
                    instructions.emit_code(Opcode::ByteEqual);
                    pop_opcode = Opcode::PopByte;
                }
                12 => {
                    // bool != bool
                    instructions.emit_code(Opcode::ByteEqual);
                    instructions.emit_bool_inversion();
                    pop_opcode = Opcode::PopByte;
                }
                13 => {
                    // String == String
                    instructions.emit_code(Opcode::StringEqual);
                }
                14 => {
                    // String != String
                    instructions.emit_code(Opcode::StringEqual);
                    instructions.emit_bool_inversion();
                }
                15 => instructions.emit_code(Opcode::IntEqual),
                16 => {
                    // Int != Int
                    instructions.emit_code(Opcode::IntEqual);
                    instructions.emit_bool_inversion();
                }
                17 => instructions.emit_code(Opcode::IntLessThan),
                18 => instructions.emit_code(Opcode::IntLessOrEqual),
                19 => instructions.emit_code(Opcode::IntGreaterThan),
                20 => instructions.emit_code(Opcode::IntGreaterOrEqual),
                21 => instructions.emit_code(Opcode::FloatEqual),
                22 => {
                    // Float != Float
                    instructions.emit_code(Opcode::FloatEqual);
                    instructions.emit_bool_inversion();
                }
                23 => instructions.emit_code(Opcode::FloatLessThan),
                24 => instructions.emit_code(Opcode::FloatLessOrEqual),
                25 => instructions.emit_code(Opcode::FloatGreaterThan),
                26 => instructions.emit_code(Opcode::FloatGreaterOrEqual),
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
            let jump_to_else = instructions.emit_jump(Opcode::IfNotJump);
            instructions.emit_push_constant_ref(true_string);
            let jump_to_end = instructions.emit_jump(Opcode::Jump);
            instructions.patch_jump(jump_to_else);
            instructions.emit_push_constant_ref(false_string);
            instructions.patch_jump(jump_to_end);
        }
        28 => {
            // ExitCode -> String
            instructions.emit_code(Opcode::ConvertByteToInt);
            instructions.emit_code(Opcode::ConvertIntToStr);
        }
        29 => {
            // Int -> String
            instructions.emit_code(Opcode::ConvertIntToStr);
        }
        30 => {
            // Float -> String
            instructions.emit_code(Opcode::ConvertFloatToStr);
        }
        33 => {
            // String + String -> String
            emit(
                args.get(0)
                    .expect("Cannot concatenate a string without a second string"),
                instructions,
                typing,
                engine,
                cp,
                state,
            );
            instructions.emit_code(Opcode::Concat);
        }
        id => todo!("Native function with id {id}"),
    };

    if !state.use_values {
        instructions.emit_code(pop_opcode);
    }
}
