use analyzer::engine::Engine;
use analyzer::relations::NativeId;
use analyzer::types::hir::TypedExpr;
use analyzer::types::Typing;

use crate::bytecode::{Instructions, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState};
use crate::locals::LocalsLayout;
use crate::r#type::ValueStackSize;

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
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last_used = state.use_values(true);
    emit(callee, instructions, typing, engine, cp, locals, state);

    let pushed_size = match native.0 {
        0 => {
            // ExitCode -> Bool
            instructions.emit_bool_inversion();
            ValueStackSize::Byte
        }
        1..=9 => {
            // Arithmetic expression
            emit(
                args.get(0).expect("A binary expression takes two operands"),
                instructions,
                typing,
                engine,
                cp,
                locals,
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
            ValueStackSize::QWord
        }
        10 => {
            // !bool
            instructions.emit_bool_inversion();
            ValueStackSize::Byte
        }
        11..=26 => {
            // Comparison expression
            emit(
                args.get(0).expect("A comparison takes two operands"),
                instructions,
                typing,
                engine,
                cp,
                locals,
                state,
            );

            match native.0 {
                11 => {
                    // bool == bool
                    instructions.emit_code(Opcode::BXor);
                    instructions.emit_bool_inversion();
                }
                12 => {
                    // bool != bool
                    instructions.emit_code(Opcode::BXor);
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
            ValueStackSize::Byte
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
            ValueStackSize::Reference
        }
        28 => {
            // ExitCode -> String
            instructions.emit_code(Opcode::ConvertByteToInt);
            instructions.emit_code(Opcode::ConvertIntToStr);
            ValueStackSize::Reference
        }
        29 => {
            // Int -> String
            instructions.emit_code(Opcode::ConvertIntToStr);
            ValueStackSize::Reference
        }
        30 => {
            // Float -> String
            instructions.emit_code(Opcode::ConvertFloatToStr);
            ValueStackSize::Reference
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
                locals,
                state,
            );
            instructions.emit_code(Opcode::Concat);
            ValueStackSize::Reference
        }
        id => todo!("Native function with id {id}"),
    };

    // restore last state of use_values
    state.use_values(last_used);

    if !state.use_values {
        instructions.emit_pop(pushed_size);
    }
}
