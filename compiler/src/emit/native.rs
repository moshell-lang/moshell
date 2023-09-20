use analyzer::relations::NativeId;
use analyzer::types::hir::MethodCall;
use analyzer::types::ty::TypeRef;

use crate::bytecode::{Instructions, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, is_boxable_primitive, EmissionState, EmitterContext};
use crate::locals::LocalsLayout;
use crate::r#type::ValueStackSize;

const STRING_EQ: &str = "lang::String::eq";
const STRING_CONCAT: &str = "lang::String::concat";
const INT_TO_STRING: &str = "lang::Int::to_string";
const FLOAT_TO_STRING: &str = "lang::Float::to_string";
const VEC_INDEX: &str = "lang::Vec::[]";
const VEC_POP: &str = "lang::Vec::pop";
const VEC_PUSH: &str = "lang::Vec::push";
const VEC_LEN: &str = "lang::Vec::len";
const STRING_SPLIT: &str = "lang::String::split";
const STRING_BYTES: &str = "lang::String::bytes";

/// Emits a primitive sequence of instructions.
#[allow(clippy::too_many_arguments)]
pub(crate) fn emit_natives(
    native: NativeId,
    MethodCall {
        callee,
        arguments: args,
        ..
    }: &MethodCall,
    receiver_ty: TypeRef,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last_used = state.use_values(true);
    emit(callee, instructions, ctx, cp, locals, state);

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
                ctx,
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
                ctx,
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
                    instructions.emit_invoke(cp.insert_string(STRING_EQ));
                }
                14 => {
                    // String != String
                    instructions.emit_invoke(cp.insert_string(STRING_EQ));
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
            ValueStackSize::QWord
        }
        28 => {
            // ExitCode -> String
            instructions.emit_code(Opcode::ConvertByteToInt);
            instructions.emit_invoke(cp.insert_string(INT_TO_STRING));
            ValueStackSize::QWord
        }
        29 => {
            // Int -> String
            instructions.emit_invoke(cp.insert_string(INT_TO_STRING));
            ValueStackSize::QWord
        }
        30 => {
            // Float -> String
            instructions.emit_invoke(cp.insert_string(FLOAT_TO_STRING));
            ValueStackSize::QWord
        }
        33 => {
            // String + String -> String
            emit(
                args.get(0)
                    .expect("Cannot concatenate a string without a second string"),
                instructions,
                ctx,
                cp,
                locals,
                state,
            );
            instructions.emit_invoke(cp.insert_string(STRING_CONCAT));
            ValueStackSize::QWord
        }
        34 => {
            // vector[Int] -> T
            emit(
                args.get(0).expect("Cannot index a vector without an index"),
                instructions,
                ctx,
                cp,
                locals,
                state,
            );
            instructions.emit_invoke(cp.insert_string(VEC_INDEX));
            if state.use_values
                && is_boxable_primitive(ctx.get_type(receiver_ty).expect("Invalid type"))
            {
                instructions.emit_code(Opcode::Unbox);
            }
            ValueStackSize::QWord
        }
        35 => {
            // vector.push(T)
            let first = args
                .first()
                .expect("Cannot push to a vector without a value");
            emit(first, instructions, ctx, cp, locals, state);
            if state.use_values
                && is_boxable_primitive(ctx.get_type(first.ty).expect("Invalid type"))
            {
                instructions.emit_code(Opcode::BoxInt);
            }
            instructions.emit_invoke(cp.insert_string(VEC_PUSH));
            ValueStackSize::Zero
        }
        36 => {
            // vector.pop() T
            instructions.emit_invoke(cp.insert_string(VEC_POP));
            if state.use_values
                && is_boxable_primitive(ctx.get_type(receiver_ty).expect("Invalid type"))
            {
                instructions.emit_code(Opcode::Unbox);
            }
            ValueStackSize::QWord
        }
        37 => {
            // vector.len()
            instructions.emit_invoke(cp.insert_string(VEC_LEN));
            ValueStackSize::QWord
        }
        38 => {
            // string.split(delim)
            emit(
                args.get(0)
                    .expect("Cannot split a string without a delimiter"),
                instructions,
                ctx,
                cp,
                locals,
                state,
            );
            instructions.emit_invoke(cp.insert_string(STRING_SPLIT));
            ValueStackSize::QWord
        }
        39 => {
            // string.bytes()
            instructions.emit_invoke(cp.insert_string(STRING_BYTES));
            ValueStackSize::QWord
        }
        40 => {
            // Int -> Exitcode
            instructions.emit_code(Opcode::ConvertIntToByte);
            ValueStackSize::Byte
        }
        41 => {
            // Exitcode -> Int
            instructions.emit_code(Opcode::ConvertByteToInt);
            ValueStackSize::QWord
        }
        id => todo!("Native function with id {id}"),
    };

    // restore last state of use_values
    state.use_values(last_used);

    if !state.use_values {
        instructions.emit_pop(pushed_size);
    }
}
