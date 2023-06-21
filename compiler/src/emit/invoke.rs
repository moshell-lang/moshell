use analyzer::engine::Engine;
use analyzer::relations::Definition;
use analyzer::types::hir::{FunctionCall, TypeId, TypedExpr};
use analyzer::types::*;

use crate::bytecode::{Instructions, Opcode};
use crate::constant_pool::{ConstantPool, FunctionSignature};
use crate::emit;
use crate::emit::EmissionState;
use crate::r#type::{get_type_size, TypeSize};

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

    // subtract from current operand stack position the string arguments
    instructions.current_operand_stack_pos -= TypeSize::QWord as u32 * arguments.len() as u32;

    instructions.emit_code(Opcode::Spawn);
    instructions.bytecode.emit_byte(arguments.len() as u8);
    instructions.extend_operands_size(TypeSize::Byte);

    if !use_return {
        // The Spawn operation will push the process's exitcode onto the stack
        // in order to maintain the stack's size, we instantly pop
        // the stack if the value isn't used later in the code
        instructions.emit_pop_byte();
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
    let mut arguments_total_size = 0;

    for arg in &function_call.arguments {
        emit(arg, instructions, typing, engine, cp, state);
        arguments_total_size += get_type_size(&arg.ty) as u32
    }
    // remove from stack total position the total size of arguments as the `Invoke` opcode
    // will pop all the arguments to make the invocation
    instructions.current_operand_stack_pos -= arguments_total_size;

    let params = function_call.arguments.iter().map(|e| e.ty);
    let name = match function_call.definition {
        Definition::User(u) => engine.get_environment(u).unwrap().fqn.to_string(),
        Definition::Native(_) => todo!("native call to functions are not supported"),
    };
    let signature = FunctionSignature::make(&name, params, return_type, typing, cp);
    instructions.emit_code(Opcode::Invoke);
    instructions
        .bytecode
        .emit_constant_ref(cp.insert_signature(signature));
    //the jump opcode will pop one byte from the operand stack
    instructions.extend_operands_size(&return_type);
}
