use analyzer::engine::Engine;
use analyzer::relations::Definition;
use analyzer::types::hir::{FunctionCall, Redir, Redirect, TypeId, TypedExpr};
use analyzer::types::Typing;
use ast::call::{RedirFd, RedirOp};
use libc::{O_APPEND, O_CREAT, O_RDONLY, O_RDWR, O_WRONLY};

use crate::bytecode::{Instructions, Opcode};
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
    let jump_to_parent = instructions.emit_jump(Opcode::Fork);

    for arg in arguments {
        emit(arg, instructions, typing, engine, cp, locals, state);
    }
    state.use_values(last_use);

    instructions
        .emit_exec(u8::try_from(arguments.len()).expect("too many arguments in process call"));
    instructions.patch_jump(jump_to_parent);
    instructions.emit_code(Opcode::Wait);

    // The Spawn operation will push the process's exitcode onto the stack
    if !state.use_values {
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

    for arg in &function_call.arguments {
        emit(arg, instructions, typing, engine, cp, locals, state);
    }

    state.use_values(last_used);

    let name = match function_call.definition {
        Definition::User(u) => engine.get_environment(u).unwrap().fqn.clone(),
        Definition::Native(_) => todo!("native call to functions are not supported"),
    };

    let return_type_size = return_type.into();

    let signature_idx = cp.insert_string(name);
    instructions.emit_invoke(signature_idx);

    // The Invoke operation will push the return value onto the stack
    if !state.use_values {
        // in order to maintain the stack's size, we instantly pop
        // the stack if the value isn't used later in the code
        instructions.emit_pop(return_type_size);
    }
}

pub fn emit_redirect(
    redirect: &Redirect,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    for redirection in &redirect.redirections {
        emit_redir(redirection, instructions, typing, engine, cp, locals, state);
    }
    emit(
        &redirect.expression,
        instructions,
        typing,
        engine,
        cp,
        locals,
        state,
    );
    for redir in &redirect.redirections {
        instructions.emit_code(Opcode::PopRedirect);
        if redir.fd == RedirFd::Wildcard {
            instructions.emit_code(Opcode::PopRedirect);
        }
    }
}

fn emit_redir(
    redir: &Redir,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last = state.use_values(true);
    emit(
        &redir.operand,
        instructions,
        typing,
        engine,
        cp,
        locals,
        state,
    );
    state.use_values(last);
    match redir.operator {
        RedirOp::Read => {
            instructions.emit_open(O_CREAT | O_RDONLY);
        }
        RedirOp::ReadWrite => {
            instructions.emit_open(O_CREAT | O_RDWR);
        }
        RedirOp::Write => {
            instructions.emit_open(O_CREAT | O_WRONLY);
        }
        RedirOp::Append => {
            instructions.emit_open(O_CREAT | O_WRONLY | O_APPEND);
        }
        RedirOp::FdIn | RedirOp::FdOut | RedirOp::String => {}
    }
    instructions.emit_code(Opcode::Dup);
    match redir.fd {
        RedirFd::Default => {
            instructions.emit_push_int(if redir.operator == RedirOp::Read {
                0
            } else {
                1
            });
        }
        RedirFd::Wildcard => {
            instructions.emit_code(Opcode::Dup);
            instructions.emit_push_int(1);
            instructions.emit_code(Opcode::Redirect);
            instructions.emit_push_int(2);
        }
        RedirFd::Fd(fd) => {
            instructions.emit_push_int(fd as i64);
        }
    }
    instructions.emit_code(Opcode::Redirect);
    if matches!(
        redir.operator,
        RedirOp::Read | RedirOp::ReadWrite | RedirOp::Write | RedirOp::Append
    ) {
        instructions.emit_code(Opcode::Close);
    } else {
        instructions.emit_code(Opcode::PopQWord);
    }
}
