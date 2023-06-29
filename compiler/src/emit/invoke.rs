use analyzer::engine::Engine;
use analyzer::relations::Definition;
use analyzer::types::hir::{FunctionCall, TypeId, TypedExpr};
use analyzer::types::*;

use crate::bytecode::Instructions;
use crate::constant_pool::ConstantPool;
use crate::emit;
use crate::emit::EmissionState;
use crate::locals::LocalsLayout;
use crate::r#type::ValueStackSize;

pub fn emit_process_call(
    arguments: &Vec<TypedExpr>,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last_use = state.use_values(true);
    let last_returns = state.returning_value(false);

    for arg in arguments {
        emit(arg, instructions, typing, engine, cp, locals, state);
    }
    state.use_values(last_use);
    state.returning_value(last_returns);

    instructions.emit_spawn(arguments.len() as u8);

    // The Spawn operation will push the process's exitcode onto the stack
    if state.is_returning_value {
        // The value of this spawn is returned
        instructions.emit_set_return(ValueStackSize::Byte);
    } else if !state.use_values {
        // in order to maintain the stack's size, we instantly pop
        // the stack if the value isn't used later in the code
        instructions.emit_pop(ValueStackSize::Byte);
    }
}

#[allow(clippy::too_many_arguments)]
pub fn emit_function_call(
    function_call: &FunctionCall,
    return_type: TypeId,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last_used = state.use_values(true);
    let last_returns = state.returning_value(false);

    for arg in &function_call.arguments {
        emit(arg, instructions, typing, engine, cp, locals, state);
    }

    state.use_values(last_used);
    state.returning_value(last_returns);

    let name = match function_call.definition {
        Definition::User(u) => engine.get_environment(u).unwrap().fqn.clone(),
        Definition::Native(_) => todo!("native call to functions are not supported"),
    };

    let return_type_size = return_type.into();

    let signature_idx = cp.insert_string(name);
    instructions.emit_invoke(signature_idx);

    // The Invoke operation will push the return value onto the stack
    if state.is_returning_value {
        // The value of this invocation is returned
        instructions.emit_set_return(return_type_size);
    } else if !state.use_values {
        // in order to maintain the stack's size, we instantly pop
        // the stack if the value isn't used later in the code
        instructions.emit_pop(return_type_size);
    }
}
