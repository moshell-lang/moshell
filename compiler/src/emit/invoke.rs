use analyzer::types::hir::{FunctionCall, TypedExpr, TypeId};
use analyzer::types::*;

use crate::bytecode::{Instructions, Opcode};
use crate::constant_pool::{ConstantPool, FunctionSignature};
use crate::emit;
use crate::emit::EmissionState;

pub fn emit_process_call(
    arguments: &Vec<TypedExpr>,
    use_return: bool,
    instructions: &mut Instructions,
    typing: &Typing,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    for arg in arguments {
        emit(arg, instructions, typing, cp, state);

        // convert argument to string if needed
        match arg.ty {
            INT => instructions.emit_code(Opcode::ConvertIntToStr),
            FLOAT => instructions.emit_code(Opcode::ConvertFloatToStr),
            STRING => {}
            _ => todo!("Convert to other types"),
        }
    }

    instructions.emit_code(Opcode::Spawn);
    instructions.bytecode.emit_byte(arguments.len() as u8);

    if !use_return {
        // The Spawn operation will push the process's exitcode onto the stack
        // in order to maintain the stack's size, we instantly pop
        // the stack if the value isn't used later in the code
        instructions.emit_code(Opcode::PopByte)
    }
}

pub fn emit_function_call(
    function_call: &FunctionCall,
    return_type: TypeId,
    instructions: &mut Instructions,
    typing: &Typing,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {

    for arg in &function_call.arguments {
        emit(arg, instructions, typing, cp, state);
    }

    let params = function_call.arguments.iter().map(|e| e.ty);
    let signature = FunctionSignature::make(&function_call.name, params, return_type, typing, cp);
    instructions.emit_code(Opcode::Invoke);
    instructions.bytecode.emit_constant_ref(cp.insert_signature(signature))
}
