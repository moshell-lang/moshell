use analyzer::engine::Engine;
use analyzer::relations::Definition;
use analyzer::types::hir::{ExprKind, FunctionCall, Redir, Redirect, TypeId, TypedExpr};
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
    let jump_to_parent = instructions.emit_jump(Opcode::Fork);
    emit_process_call_self(arguments, instructions, typing, engine, cp, locals, state);
    instructions.patch_jump(jump_to_parent);
    instructions.emit_code(Opcode::Wait);

    if !state.use_values {
        // The Spawn operation will push the process's exitcode onto the stack
        // in order to maintain the stack's size, we instantly pop
        // the stack if the value isn't used later in the code
        instructions.emit_pop(ValueStackSize::Byte);
    }
}

fn emit_process_call_self(
    arguments: &Vec<TypedExpr>,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last_use = state.use_values(true);
    for arg in arguments {
        emit(arg, instructions, typing, engine, cp, locals, state);
    }
    state.use_values(last_use);

    instructions
        .emit_exec(u8::try_from(arguments.len()).expect("too many arguments in process call"));
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

/// Create a pipelines of command, where each part is a separated process created
/// with a Fork instruction.
///
/// Each process is connected to the next one with a pipe, it writes to the pipe
/// and the next process reads from it. The first process is connected to the
/// current process's stdin and the last process is connected to the current
/// process's stdout. After each process is launched, the parent process waits
/// for them to finish, and returns the exit code of the last process, or the
/// exit code of the first failing process.
pub fn emit_pipeline(
    pipeline: &Vec<TypedExpr>,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    // Pipelines work by creating N - 1 pipes between the N commands.
    // Two pipes may have to be kept on top of the stack at the same time,
    // so we need to close them as soon as possible.
    assert!(
        pipeline.len() > 1,
        "Cannot compile pipeline with less than 2 commands"
    );
    let last = state.use_values(true);
    let (first, commands) = pipeline.split_first().expect("Empty pipeline");

    instructions.emit_code(Opcode::Pipe);

    // First process:
    let jump_to_parent = instructions.emit_jump(Opcode::Fork);
    // Bound this children process stdout to the writing end of the pipe, that is on top of the stack
    instructions.emit_push_int(1);
    instructions.emit_code(Opcode::Redirect);
    instructions.emit_code(Opcode::Close); // Close the pipe's reading end, since we don't need it

    // Avoid forking a second time since we are already in a child process
    if let ExprKind::ProcessCall(process_args) = &first.kind {
        emit_process_call_self(
            process_args,
            instructions,
            typing,
            engine,
            cp,
            locals,
            state,
        );
    } else {
        emit(first, instructions, typing, engine, cp, locals, state);
    }

    instructions.patch_jump(jump_to_parent);

    // Create the rest of the pipeline
    let mut it = commands.iter().enumerate().peekable();
    while let Some((window, command)) = it.next() {
        instructions.emit_code(Opcode::Swap);
        instructions.emit_code(Opcode::Close);

        if window > 0 {
            // Do a delayed close of the previous pipe, since we don't need it anymore.
            // In a complex pipeline, two pipes coexists on the stack, and the reading end of the
            // previous pipe is still needed by the next process. Since it is deep in the stack,
            // we need to get it back on top of the stack.
            instructions.emit_code(Opcode::Swap2);
            instructions.emit_code(Opcode::Close);
        }

        instructions.emit_code(Opcode::Swap);
        if it.peek().is_some() {
            // Only create a pipe if this is not the last process
            instructions.emit_code(Opcode::Pipe);
        }

        let jump_to_parent = instructions.emit_jump(Opcode::Fork);

        if it.peek().is_some() {
            // Bound this children process stdout to the writing end of the pipe, that is on top of the stack
            instructions.emit_push_int(1);
            instructions.emit_code(Opcode::Redirect);
            instructions.emit_code(Opcode::Close); // Close the pipe's reading end, since we don't need it
        }

        // Bound this children process stdin to the reading end of the pipe, that is on top of the stack
        instructions.emit_push_int(0);
        instructions.emit_code(Opcode::Redirect);

        // Avoid forking a second time since we are already in a child process
        if let ExprKind::ProcessCall(process_args) = &command.kind {
            emit_process_call_self(
                process_args,
                instructions,
                typing,
                engine,
                cp,
                locals,
                state,
            );
        } else {
            emit(command, instructions, typing, engine, cp, locals, state);
        }

        instructions.patch_jump(jump_to_parent);
    }
    state.use_values(last);

    // Close the remaining pipe reading end
    instructions.emit_code(Opcode::Swap);
    instructions.emit_code(Opcode::Close);

    assert!(!state.use_values, "Pipeline values cannot be used yet");
    for _ in 0..pipeline.len() {
        instructions.emit_code(Opcode::Wait);
        instructions.emit_code(Opcode::PopByte);
    }
}
