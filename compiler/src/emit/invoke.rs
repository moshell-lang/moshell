use libc::{O_APPEND, O_CREAT, O_RDONLY, O_RDWR, O_WRONLY};

use analyzer::relations::Definition;
use analyzer::types::hir::{ExprKind, FunctionCall, Redir, Redirect, TypedExpr, Var};
use analyzer::types::ty::TypeRef;
use ast::call::{RedirFd, RedirOp};

use crate::bytecode::{Instructions, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit;
use crate::emit::{EmissionState, EmitterContext};
use crate::locals::LocalsLayout;
use crate::r#type::ValueStackSize;

/// Emit any expression, knowing that we are already in a forked process.
///
/// Avoid forking another time if we can replace the current process.

pub fn emit_already_forked(
    expr: &TypedExpr,
    instructions: &mut Instructions,
    ctx: EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    match &expr.kind {
        ExprKind::ProcessCall(process_args) => {
            emit_process_call_self(process_args, instructions, ctx, cp, locals, state);
        }
        ExprKind::Redirect(Redirect {
            expression,
            redirections,
        }) => {
            for redirection in redirections {
                emit_redir_self(
                    redirection,
                    instructions,
                    ctx,
                    cp,
                    locals,
                    state,
                    Opcode::Redirect,
                );
            }
            emit_already_forked(expression, instructions, ctx, cp, locals, state);
        }
        ExprKind::Block(block) => {
            if let Some((last, block)) = block.split_last() {
                for expr in block {
                    emit(expr, instructions, ctx, cp, locals, state);
                }
                emit_already_forked(last, instructions, ctx, cp, locals, state);
            }
        }
        _ => {
            emit(expr, instructions, ctx, cp, locals, state);
        }
    }
}

pub fn emit_process_end(last: Option<&TypedExpr>, instructions: &mut Instructions) {
    match last {
        Some(TypedExpr {
            kind: ExprKind::ProcessCall(_),
            ..
        }) => {}
        Some(TypedExpr {
            kind: ExprKind::Redirect(redirected),
            ..
        }) if matches!(redirected.expression.kind, ExprKind::ProcessCall(_)) => {}
        _ => {
            instructions.emit_push_byte(0);
            instructions.emit_code(Opcode::Exit);
        }
    }
}

pub fn emit_process_call(
    arguments: &Vec<TypedExpr>,
    instructions: &mut Instructions,
    ctx: EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let jump_to_parent = instructions.emit_jump(Opcode::Fork);
    emit_process_call_self(arguments, instructions, ctx, cp, locals, state);
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
    ctx: EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last_use = state.use_values(true);
    for arg in arguments {
        emit(arg, instructions, ctx, cp, locals, state);
    }
    state.use_values(last_use);

    instructions
        .emit_exec(u8::try_from(arguments.len()).expect("too many arguments in process call"));
}

pub fn emit_function_invocation(
    function_call: &FunctionCall,
    return_type: TypeRef,
    instructions: &mut Instructions,
    ctx: EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last_used = state.use_values(true);

    for arg in &function_call.arguments {
        emit(arg, instructions, ctx, cp, locals, state);
    }

    state.use_values(last_used);

    let (env, captures) = match function_call.definition {
        Definition::User(id) => {
            let captures = ctx.captures[id.0]
                .as_ref()
                .expect("captures not set during function invocation emission");
            let env = ctx.engine.get_environment(id).unwrap();
            (env, captures)
        }
        Definition::Native(_) => {
            todo!("native call to functions are not supported")
        }
    };

    for capture in captures {
        if capture.source == ctx.chunk_id {
            // if its a local value hosted by the caller frame, create a reference
            // to the value
            instructions.emit_push_stack_ref(Var::Local(capture.object_id), locals);
        } else {
            // if its a captured variable, get the reference's value from locals
            instructions.emit_push_stack_ref(Var::External(*capture), locals);
            instructions.emit_code(Opcode::GetRefQWord);
        }
    }

    let return_type_size = return_type.into();

    let signature_idx = cp.insert_string(&env.fqn);
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
    ctx: EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    for redirection in &redirect.redirections {
        emit_redir(redirection, instructions, ctx, cp, locals, state);
    }
    emit(&redirect.expression, instructions, ctx, cp, locals, state);
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
    ctx: EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    emit_redir_self(
        redir,
        instructions,
        ctx,
        cp,
        locals,
        state,
        Opcode::SetupRedirect,
    );
}

fn emit_redir_self(
    redir: &Redir,
    instructions: &mut Instructions,
    ctx: EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
    redir_code: Opcode,
) {
    debug_assert!(matches!(
        redir_code,
        Opcode::SetupRedirect | Opcode::Redirect
    ));
    if redir.operator == RedirOp::String {
        instructions.emit_code(Opcode::Pipe);
    }
    let last = state.use_values(true);
    emit(&redir.operand, instructions, ctx, cp, locals, state);
    state.use_values(last);
    match redir.operator {
        RedirOp::Read => {
            instructions.emit_open(O_RDONLY);
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
        RedirOp::String => {
            instructions.emit_code(Opcode::Write);
        }
        RedirOp::FdIn | RedirOp::FdOut => {}
    }
    match redir.fd {
        RedirFd::Default => {
            instructions.emit_push_int(
                if matches!(
                    redir.operator,
                    RedirOp::Read | RedirOp::ReadWrite | RedirOp::FdIn | RedirOp::String
                ) {
                    0
                } else {
                    1
                },
            );
        }
        RedirFd::Wildcard => {
            instructions.emit_push_int(1);
            instructions.emit_code(redir_code);
            instructions.emit_push_int(2);
        }
        RedirFd::Fd(fd) => {
            instructions.emit_push_int(fd as i64);
        }
    }
    instructions.emit_code(redir_code);
    if matches!(
        redir.operator,
        RedirOp::Read | RedirOp::ReadWrite | RedirOp::Write | RedirOp::Append | RedirOp::String
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
    ctx: EmitterContext,
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
    instructions.emit_code(Opcode::Close); // Close the pipe's writing end, that we just bound to stdout
    instructions.emit_code(Opcode::Close); // Close the pipe's reading end, since we don't need it

    emit_already_forked(first, instructions, ctx, cp, locals, state);
    emit_process_end(Some(first), instructions);

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
            instructions.emit_code(Opcode::Close); // Close the pipe's writing end, that we just bound to stdout
            instructions.emit_code(Opcode::Close); // Close the pipe's reading end, since we don't need it
        }

        // Bound this children process stdin to the reading end of the pipe, that is on top of the stack
        instructions.emit_push_int(0);
        instructions.emit_code(Opcode::Redirect);
        instructions.emit_code(Opcode::Close); // Close the pipe's reading end, since we just bound it to stdin

        emit_already_forked(command, instructions, ctx, cp, locals, state);
        emit_process_end(Some(command), instructions);

        instructions.patch_jump(jump_to_parent);
    }
    state.use_values(last);

    // Close the remaining pipe reading end
    instructions.emit_code(Opcode::Swap);
    instructions.emit_code(Opcode::Close);

    // Get the exit code of the last process, and wait every other processes
    if state.use_values {
        instructions.emit_code(Opcode::Wait);
        // Convert the exit code to a int to be able to swap it
        instructions.emit_code(Opcode::ConvertByteToInt);
        for _ in 1..pipeline.len() {
            instructions.emit_code(Opcode::Swap);
            instructions.emit_code(Opcode::Wait);
            instructions.emit_code(Opcode::DupByte);

            // If the exit code is 0, keep the previous exit code, otherwise replace it
            let jump_to_else = instructions.emit_jump(Opcode::IfJump);
            instructions.emit_code(Opcode::PopByte);
            let jump_to_end = instructions.emit_jump(Opcode::Jump);
            instructions.patch_jump(jump_to_else);
            instructions.emit_code(Opcode::ConvertByteToInt);
            instructions.emit_code(Opcode::Swap);
            instructions.emit_code(Opcode::PopQWord);
            instructions.patch_jump(jump_to_end);
        }
        instructions.emit_code(Opcode::ConvertIntToByte);
    } else {
        for _ in 0..pipeline.len() {
            instructions.emit_code(Opcode::Wait);
            instructions.emit_code(Opcode::PopByte);
        }
    }
}

pub fn emit_capture(
    commands: &[TypedExpr],
    instructions: &mut Instructions,
    ctx: EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    instructions.emit_code(Opcode::Pipe);
    let jump_to_parent = instructions.emit_jump(Opcode::Fork);
    instructions.emit_push_int(1);
    instructions.emit_code(Opcode::Redirect);
    instructions.emit_code(Opcode::Close);
    instructions.emit_code(Opcode::Close);
    let last = state.use_values(false);
    if let Some((last, commands)) = commands.split_last() {
        for command in commands {
            emit(command, instructions, ctx, cp, locals, state);
        }
        emit_already_forked(last, instructions, ctx, cp, locals, state);
    }
    state.use_values(last);
    emit_process_end(commands.last(), instructions);

    instructions.patch_jump(jump_to_parent);
    instructions.emit_code(Opcode::Swap);
    instructions.emit_code(Opcode::Close);
    instructions.emit_code(Opcode::Swap);
    instructions.emit_code(Opcode::Read);
    instructions.emit_code(Opcode::Swap);
    instructions.emit_code(Opcode::Wait);
    instructions.emit_code(Opcode::PopByte);

    if !state.use_values {
        instructions.emit_code(Opcode::PopQWord);
    }
}
