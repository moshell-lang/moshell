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
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    let last = state.use_values(true);
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
    state.use_values(last);

    instructions.emit_spawn(arguments.len() as u8);

    if !state.use_values {
        // The Spawn operation will push the process's exitcode onto the stack
        // in order to maintain the stack's size, we instantly pop
        // the stack if the value isn't used later in the code
        instructions.emit_code(Opcode::PopByte);
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
    let last = state.use_values(true);
    for arg in &function_call.arguments {
        emit(arg, instructions, typing, engine, cp, state);
    }
    state.use_values(last);

    let args_types: Vec<_> = function_call.arguments
        .iter()
        .map(|e| e.ty)
        .collect();

    let name = match function_call.definition {
        Definition::User(u) => {
            engine.get_environment(u)
                .unwrap()
                .fqn
                .to_string()
        },
        Definition::Native(_) => todo!("native call to functions are not supported"),
    };

    let signature = FunctionSignature::make(&name, &args_types, return_type, typing, cp);
    let signature_idx = cp.insert_signature(signature);
    instructions.emit_invoke(signature_idx);
}
