use analyzer::engine::Engine;
use analyzer::relations::Definition;
use analyzer::types::hir::{FunctionCall, TypeId, TypedExpr};
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
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    for arg in arguments {
        emit(arg, instructions, typing, engine, cp, state);

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
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    for arg in &function_call.arguments {
        emit(arg, instructions, typing, engine, cp, state);
    }

    let params = function_call.arguments.iter().map(|e| e.ty);
    let name = match function_call.definition {
        Definition::User(u) => engine.get_environment(u).unwrap().fqn.to_string(),
        Definition::Native(_) => todo!("native call to functions are not supported"),
    };
    let signature = FunctionSignature::make(&name, params, return_type, typing, cp);
    instructions.emit_code(Opcode::Invoke);
    instructions
        .bytecode
        .emit_constant_ref(cp.insert_signature(signature))
}
