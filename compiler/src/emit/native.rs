use crate::bytecode::{Instructions, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState, EmitterContext};
use crate::locals::LocalsLayout;
use crate::r#type::ValueStackSize;
use analyzer::hir::MethodCall;
use analyzer::typing::function::FunctionKind;
use analyzer::typing::user;
use analyzer::typing::user::TypeId;

const STRING_EQ: &str = "lang::String::eq";
const STRING_CONCAT: &str = "lang::String::concat";
const INT_TO_STRING: &str = "lang::Int::to_string";
const FLOAT_TO_STRING: &str = "lang::Float::to_string";
pub(super) const STRING_LEN: &str = "lang::String::len";
pub(super) const STRING_INDEX: &str = "lang::String::[]";
pub(super) const VEC_INDEX: &str = "lang::Vec::[]";
const VEC_INDEX_EQ: &str = "lang::Vec::[]=";
const VEC_POP: &str = "lang::Vec::pop";
pub(super) const VEC_PUSH: &str = "lang::Vec::push";
pub(super) const VEC_EXTEND: &str = "lang::Vec::extend";
pub(super) const VEC_LEN: &str = "lang::Vec::len";
const VEC_POP_HEAD: &str = "lang::Vec::pop_head";
const STRING_SPLIT: &str = "lang::String::split";
const STRING_BYTES: &str = "lang::String::bytes";
const GLOB_EXPAND: &str = "lang::glob::expand";

/// Emits a primitive sequence of instructions.
pub(crate) fn emit_natives(
    MethodCall {
        callee,
        arguments: args,
        function_id,
    }: &MethodCall,
    receiver_ty: TypeId,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let function = &ctx.registry[*function_id];
    match function.kind {
        FunctionKind::Intrinsic => {
            let name = function.fqn.as_os_str().to_str().unwrap();
            let uses = state.use_values(true);
            emit(callee, instructions, ctx, cp, locals, state);
            if name == "Bool/and" || name == "Exitcode/and" {
                instructions.emit_code(Opcode::DupByte);
                let end_jump = instructions.emit_jump(if name == "Exitcode/and" {
                    Opcode::IfJump
                } else {
                    Opcode::IfNotJump
                });
                instructions.emit_pop(ValueStackSize::Byte);
                emit(
                    args.first()
                        .expect("Cannot AND a boolean without a second boolean"),
                    instructions,
                    ctx,
                    cp,
                    locals,
                    state,
                );
                instructions.patch_jump(end_jump);
                state.use_values(uses);
                return;
            } else if name == "Bool/or" || name == "Exitcode/or" {
                instructions.emit_code(Opcode::DupByte);
                let else_jump = instructions.emit_jump(if name == "Exitcode/or" {
                    Opcode::IfJump
                } else {
                    Opcode::IfNotJump
                });
                let end_jump = instructions.emit_jump(Opcode::Jump);
                instructions.patch_jump(else_jump);
                instructions.emit_pop(ValueStackSize::Byte);
                emit(
                    args.first()
                        .expect("Cannot OR a boolean without a second boolean"),
                    instructions,
                    ctx,
                    cp,
                    locals,
                    state,
                );
                instructions.patch_jump(end_jump);
                state.use_values(uses);
                return;
            }
            for arg in args {
                emit(arg, instructions, ctx, cp, locals, state);
            }
            state.use_values(uses);
            match name {
                // STRING_EQ => {
                //     todo!("Emit string equality")
                // }
                // STRING_CONCAT => {
                //     todo!("Emit string concatenation")
                // }
                // INT_TO_STRING => {
                //     todo!("Emit int to string")
                // }
                // FLOAT_TO_STRING => {
                //     todo!("Emit float to string")
                // }
                // STRING_LEN => {
                //     todo!("Emit string length")
                // }
                // STRING_INDEX => {
                //     todo!("Emit string index")
                // }
                // VEC_INDEX => {
                //     todo!("Emit vec index")
                // }
                // VEC_INDEX_EQ => {
                //     todo!("Emit vec index assignment")
                // }
                // VEC_POP => {
                //     todo!("Emit vec pop")
                // }
                // VEC_PUSH => {
                //     todo!("Emit vec push")
                // }
                // VEC_EXTEND => {
                //     todo!("Emit vec extend")
                // }
                // VEC_LEN => {
                //     todo!("Emit vec length")
                // }
                // VEC_POP_HEAD => {
                //     todo!("Emit vec pop head")
                // }
                // STRING_SPLIT => {
                //     todo!("Emit string split")
                // }
                // STRING_BYTES => {
                //     todo!("Emit string bytes")
                // }
                "Bool/not" => {
                    instructions.emit_bool_inversion();
                }
                "Bool/eq" => {
                    instructions.emit_code(Opcode::BXor);
                    instructions.emit_bool_inversion();
                }
                "Bool/ne" => {
                    instructions.emit_code(Opcode::BXor);
                }
                "Int/add" => {
                    instructions.emit_code(Opcode::IntAdd);
                }
                "Int/sub" => {
                    instructions.emit_code(Opcode::IntSub);
                }
                "Int/mul" => {
                    instructions.emit_code(Opcode::IntMul);
                }
                "Int/div" => {
                    instructions.emit_code(Opcode::IntDiv);
                }
                "Int/mod" => {
                    instructions.emit_code(Opcode::IntMod);
                }
                "Int/eq" => {
                    instructions.emit_code(Opcode::IntEqual);
                }
                "Int/ne" => {
                    instructions.emit_code(Opcode::IntEqual);
                    instructions.emit_bool_inversion();
                }
                "Int/lt" => {
                    instructions.emit_code(Opcode::IntLessThan);
                }
                "Int/le" => {
                    instructions.emit_code(Opcode::IntLessOrEqual);
                }
                "Int/gt" => {
                    instructions.emit_code(Opcode::IntGreaterThan);
                }
                "Int/ge" => {
                    instructions.emit_code(Opcode::IntGreaterOrEqual);
                }
                "Int/to_exitcode" => {
                    instructions.emit_code(Opcode::ConvertByteToInt);
                }
                "Int/to_string" => {
                    instructions.emit_invoke(cp.insert_string(INT_TO_STRING));
                }
                "String/eq" => {
                    instructions.emit_invoke(cp.insert_string(STRING_EQ));
                }
                "String/ne" => {
                    instructions.emit_invoke(cp.insert_string(STRING_EQ));
                    instructions.emit_bool_inversion();
                }
                "String/len" => {
                    instructions.emit_invoke(cp.insert_string(STRING_LEN));
                }
                "String/add" | "String/concat" => {
                    instructions.emit_invoke(cp.insert_string(STRING_CONCAT));
                }
                "String/[]" => {
                    instructions.emit_invoke(cp.insert_string(STRING_INDEX));
                }
                "String/split" => {
                    instructions.emit_invoke(cp.insert_string(STRING_SPLIT));
                }
                "String/bytes" => {
                    instructions.emit_invoke(cp.insert_string(STRING_BYTES));
                }
                "Vec/len" => {
                    instructions.emit_invoke(cp.insert_string(VEC_LEN));
                }
                "Vec/[]" => {
                    instructions.emit_invoke(cp.insert_string(VEC_INDEX));
                }
                "Vec/[]=" => {
                    instructions.emit_invoke(cp.insert_string(VEC_INDEX_EQ));
                }
                "Vec/push" => {
                    instructions.emit_invoke(cp.insert_string(VEC_PUSH));
                }
                "Vec/pop" => {
                    instructions.emit_invoke(cp.insert_string(VEC_POP));
                }
                "Vec/pop_head" => {
                    instructions.emit_invoke(cp.insert_string(VEC_POP_HEAD));
                }
                "Vec/extend" => {
                    instructions.emit_invoke(cp.insert_string(VEC_EXTEND));
                }
                "Option/is_some" => {
                    instructions.emit_push_int(0);
                    instructions.emit_code(Opcode::IntEqual);
                }
                "Option/is_none" => {
                    instructions.emit_push_int(0);
                    instructions.emit_code(Opcode::IntEqual);
                    instructions.emit_bool_inversion();
                }
                "Option/unwrap" => {
                    instructions.emit_code(Opcode::Dup);
                    instructions.emit_push_int(0);
                    instructions.emit_code(Opcode::IntEqual);
                    let end_jump = instructions.emit_jump(Opcode::IfNotJump);
                    instructions.emit_push_constant_ref(cp.insert_string("Cannot unwrap `None`."));
                    instructions.emit_invoke(cp.insert_string("std::panic"));
                    instructions.patch_jump(end_jump);
                }
                "Glob/expand" => {
                    instructions.emit_invoke(cp.insert_string(GLOB_EXPAND));
                }
                _ => panic!("Unknown `{}` intrinsic", function.fqn.display()),
            }
        }
        _ => panic!(
            "Call `{}`, but it's not implemented yet",
            function.fqn.display()
        ),
    }
}

pub(crate) fn emit_cast(from: TypeId, to: TypeId, instructions: &mut Instructions) {
    match (from, to) {
        (user::EXITCODE_TYPE, user::BOOL_TYPE) => {
            instructions.emit_bool_inversion();
        }
        _ => panic!("Emit cast from {from:?} to {to:?}"),
    }
}
