use analyzer::types::engine::FunctionId;
use analyzer::types::hir::MethodCall;
use analyzer::types::ty::TypeRef;

use crate::bytecode::{Instructions, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState, EmitterContext};
use crate::locals::LocalsLayout;
use crate::r#type::ValueStackSize;

const STRING_EQ: &str = "lang::String::eq";
const STRING_CONCAT: &str = "lang::String::concat";
const INT_TO_STRING: &str = "lang::Int::to_string";
const FLOAT_TO_STRING: &str = "lang::Float::to_string";
const VEC_INDEX: &str = "lang::Vec::[]";
const VEC_INDEX_EQ: &str = "lang::Vec::[]=";
const VEC_POP: &str = "lang::Vec::pop";
pub(super) const VEC_PUSH: &str = "lang::Vec::push";
pub(super) const VEC_EXTEND: &str = "lang::Vec::extend";
const VEC_LEN: &str = "lang::Vec::len";
const VEC_POP_HEAD: &str = "lang::Vec::pop_head";
const STRING_SPLIT: &str = "lang::String::split";
const STRING_BYTES: &str = "lang::String::bytes";
const GLOB_EXPAND: &str = "lang::glob::expand";

/// Emits a primitive sequence of instructions.
#[allow(clippy::get_first, clippy::too_many_arguments)]
pub(crate) fn emit_natives(
    native: FunctionId,
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

    match native.0 {
        0 => {
            // ExitCode -> Bool
            instructions.emit_bool_inversion();
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
        }
        10 => {
            // !bool
            instructions.emit_bool_inversion();
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
            instructions.emit_invoke(cp.insert_string(INT_TO_STRING));
        }
        29 | 54 => {
            // Int -> String
            instructions.emit_invoke(cp.insert_string(INT_TO_STRING));
        }
        30 => {
            // Float -> String
            instructions.emit_invoke(cp.insert_string(FLOAT_TO_STRING));
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
        }
        35 => {
            // vector.push(T)
            let first = args
                .first()
                .expect("Cannot push to a vector without a value");
            emit(first, instructions, ctx, cp, locals, state);
            if state.use_values {
                instructions.emit_box_if_primitive(first.ty);
            }
            instructions.emit_invoke(cp.insert_string(VEC_PUSH));
        }
        36 => {
            // vector.pop() T
            instructions.emit_invoke(cp.insert_string(VEC_POP));
        }
        37 => {
            // vector.len()
            instructions.emit_invoke(cp.insert_string(VEC_LEN));
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
        }
        39 => {
            // string.bytes()
            instructions.emit_invoke(cp.insert_string(STRING_BYTES));
        }
        40 | 42 => {
            // Bool && Bool -> Bool
            instructions.emit_code(Opcode::DupByte);
            let end_jump = instructions.emit_jump(if native.0 == 42 {
                Opcode::IfJump
            } else {
                Opcode::IfNotJump
            });
            instructions.emit_pop(ValueStackSize::Byte);
            emit(
                args.get(0)
                    .expect("Cannot AND a boolean without a second boolean"),
                instructions,
                ctx,
                cp,
                locals,
                state,
            );
            instructions.patch_jump(end_jump);
        }
        41 | 43 => {
            // Bool || Bool -> Bool
            instructions.emit_code(Opcode::DupByte);
            let else_jump = instructions.emit_jump(if native.0 == 43 {
                Opcode::IfJump
            } else {
                Opcode::IfNotJump
            });
            let end_jump = instructions.emit_jump(Opcode::Jump);
            instructions.patch_jump(else_jump);
            instructions.emit_pop(ValueStackSize::Byte);
            emit(
                args.get(0)
                    .expect("Cannot OR a boolean without a second boolean"),
                instructions,
                ctx,
                cp,
                locals,
                state,
            );
            instructions.patch_jump(end_jump);
        }
        44 => {
            // -Int -> Int
            instructions.emit_code(Opcode::IntNeg);
        }
        45 => {
            // -Float -> Float
            instructions.emit_code(Opcode::FloatNeg);
        }
        46 | 47 => {
            // Option[T].is_some() -> bool
            instructions.emit_push_int(0);
            instructions.emit_code(Opcode::IntEqual);
            if native.0 == 47 {
                instructions.emit_bool_inversion();
            }
        }
        48 => {
            // Option[T].unwrap() -> T
            instructions.emit_code(Opcode::Dup);
            instructions.emit_push_int(0);
            instructions.emit_code(Opcode::IntEqual);
            let end_jump = instructions.emit_jump(Opcode::IfNotJump);
            instructions.emit_push_constant_ref(cp.insert_string("Cannot unwrap `None`."));
            instructions.emit_invoke(cp.insert_string("std::panic"));
            instructions.patch_jump(end_jump);
        }
        49 => {
            // Vec[T][int] = T
            for arg in args {
                emit(arg, instructions, ctx, cp, locals, state);
            }
            instructions.emit_box_if_primitive(args[1].ty);
            instructions.emit_invoke(cp.insert_string(VEC_INDEX_EQ));
        }
        50 => {
            // Int -> Exitcode
            instructions.emit_code(Opcode::ConvertIntToByte);
        }
        51 => {
            // Exitcode -> Int
            instructions.emit_code(Opcode::ConvertByteToInt);
        }
        52 => {
            // Vec[A]::pop_head() -> Option[A]
            instructions.emit_invoke(cp.insert_string(VEC_POP_HEAD));
        }
        53 => {
            // Glob::spread() -> Vec[String]
            instructions.emit_invoke(cp.insert_string(GLOB_EXPAND));
        }
        id => todo!("Native function with id {id}"),
    };

    let has_generic_return = matches!(native.0, 34 | 36 | 48);
    if has_generic_return && !receiver_ty.is_obj() {
        instructions.emit_code(Opcode::Unbox);
    }

    // restore last state of use_values
    state.use_values(last_used);

    if !state.use_values {
        instructions.emit_pop(ValueStackSize::from(receiver_ty));
    }
}
