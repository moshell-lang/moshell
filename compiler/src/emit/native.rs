use crate::bytecode::Instructions;
use crate::constant_pool::ConstantPool;
use crate::emit::{EmissionState, EmitterContext};
use crate::locals::LocalsLayout;
use analyzer::hir::MethodCall;
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
        ..
    }: &MethodCall,
    receiver_ty: TypeId,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    todo!("Emit native function calls")
}
