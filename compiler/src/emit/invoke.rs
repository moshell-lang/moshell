use analyzer::hir::{ExprKind, FunctionCall, Redir, Redirect, Subprocess, Substitute, TypedExpr};
use analyzer::typing::function::FunctionKind;
use analyzer::typing::registry::SchemaId;
use analyzer::typing::user::{TypeId, UserType, INT_TYPE, STRING_TYPE, VECTOR_TYPE};
use libc::{O_APPEND, O_CREAT, O_RDONLY, O_RDWR, O_TRUNC, O_WRONLY};
use std::ffi::OsStr;

use ast::call::{RedirFd, RedirOp};

use crate::bytecode::{Instructions, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit;
use crate::emit::native::{VEC_EXTEND, VEC_PUSH};
use crate::emit::{EmissionState, EmitterContext};
use crate::locals::LocalsLayout;
use crate::r#type::ValueStackSize;

const NEW_VEC: &str = "std::new_vec";

/// Emit any expression, knowing that we are already in a forked process.
///
/// Avoid forking another time if we can replace the current process.
pub fn emit_already_forked(
    expr: &TypedExpr,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
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

/// Emits a terminated block of expressions, knowing that we are already in a forked process.
pub fn emit_already_forked_block(
    block: &[TypedExpr],
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    if let Some((last, commands)) = block.split_last() {
        for command in commands {
            emit(command, instructions, ctx, cp, locals, state);
        }
        emit_already_forked(last, instructions, ctx, cp, locals, state);
    }
    emit_process_end(block.last(), instructions);
}

/// Explicitly exits the process if the process is not replaced by another one.
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
    arguments: &[TypedExpr],
    redirections: &[Redir],
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    emit_arguments(arguments, instructions, ctx, cp, locals, state);

    let jump_to_parent = instructions.emit_jump(Opcode::Fork);
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

    instructions.emit_code(Opcode::Exec);
    instructions.patch_jump(jump_to_parent);

    for local in state.opened_files.drain(..) {
        instructions.emit_get_local(local, INT_TYPE.into(), locals);
        instructions.emit_code(Opcode::Close);
    }

    // Remove the arguments from the stack, as they were only needed for the child process
    instructions.emit_code(Opcode::Swap);
    instructions.emit_pop(VECTOR_TYPE.into());

    instructions.emit_code(Opcode::Wait);

    if !state.use_values {
        // The Spawn operation will push the process's exitcode onto the stack
        // in order to maintain the stack's size, we instantly pop
        // the stack if the value isn't used later in the code
        instructions.emit_pop(ValueStackSize::Byte);
    }
}

fn emit_process_call_self(
    arguments: &[TypedExpr],
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    emit_arguments(arguments, instructions, ctx, cp, locals, state);
    instructions.emit_code(Opcode::Exec);
}

/// Emits each arguments and creates a vector containing them.
fn emit_arguments(
    arguments: &[TypedExpr],
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last_use = state.use_values(true);
    instructions.emit_invoke(cp.insert_string(NEW_VEC));
    for arg in arguments {
        instructions.emit_code(Opcode::Dup);
        emit(arg, instructions, ctx, cp, locals, state);
        if arg.ty == STRING_TYPE {
            instructions.emit_invoke(cp.insert_string(VEC_PUSH));
        } else {
            instructions.emit_invoke(cp.insert_string(VEC_EXTEND));
        }
    }
    state.use_values(last_use);
}

pub fn emit_function_invocation(
    function_call: &FunctionCall,
    call_return_type: TypeId,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last_used = state.use_values(true);

    let function = &ctx.registry[function_call.function_id];

    for (arg, parameter) in function_call.arguments.iter().zip(&function.param_types) {
        emit(arg, instructions, ctx, cp, locals, state);
        // The parameter is an object but the argument isn't: may be an argument passed to a generic parameter
        if parameter.ty.is_obj() && !arg.ty.is_obj() {
            instructions.emit_box_if_primitive(arg.ty);
        }
    }

    state.use_values(last_used);

    if function.kind == FunctionKind::Constructor {
        // current constructors implementation only supports a default, non user-defined constructor
        // which is a structure that contains the given parameters.

        let constructed_structure_id: SchemaId = match ctx.types[function.return_type] {
            UserType::Parametrized {
                schema: constructed_structure_type,
                params: _,
            } => constructed_structure_type,
            _ => panic!("constructor does not returns a structure type or structure type instance"),
        };

        // get constructor's structure fully-qualified name
        let schema = &ctx.registry[constructed_structure_id];
        let struct_fqn = &schema.name;

        let layout = &ctx.layouts[constructed_structure_id.get()];

        // initialize a new structure
        instructions.emit_new(cp.insert_string(struct_fqn));
        // Default constructor inputs structure's fields in order,
        // thus we can init it from all the pushed constructor's parameters in the operands
        instructions.emit_copy_operands(layout.total_size);
    } else {
        let signature_idx = cp.insert_string(
            function
                .fqn
                .iter()
                .map(OsStr::to_string_lossy)
                .collect::<Vec<_>>()
                .join("::"),
        );
        instructions.emit_invoke(signature_idx);
    }

    let return_type = function.return_type;
    let return_type_size = return_type.into();

    // The Invoke operation will push the return value onto the stack
    if !state.use_values {
        // in order to maintain the stack's size, we instantly pop
        // the stack if the value isn't used later in the code
        instructions.emit_pop(return_type_size);
    } else if return_type.is_obj() && !call_return_type.is_obj() {
        // The function's declared return type is an object but the call return type is not: it's a boxed return value
        instructions.emit_code(Opcode::Unbox);
    }
}

pub fn emit_redirect(
    redirect: &Redirect,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    if let ExprKind::ProcessCall(arguments) = &redirect.expression.kind {
        emit_process_call(
            arguments,
            &redirect.redirections,
            instructions,
            ctx,
            cp,
            locals,
            state,
        );
        return;
    }
    for redirection in &redirect.redirections {
        emit_redir(redirection, instructions, ctx, cp, locals, state);
    }
    emit(&redirect.expression, instructions, ctx, cp, locals, state);

    for local in state.opened_files.drain(..) {
        instructions.emit_get_local(local, INT_TYPE.into(), locals);
        instructions.emit_code(Opcode::Close);
    }
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
    ctx: &EmitterContext,
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
    ctx: &EmitterContext,
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
            instructions.emit_open(O_CREAT | O_WRONLY | O_TRUNC);
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
    pipeline: &[TypedExpr],
    instructions: &mut Instructions,
    ctx: &EmitterContext,
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
    ctx: &EmitterContext,
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
    emit_already_forked_block(commands, instructions, ctx, cp, locals, state);
    state.use_values(last);

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

/// Creates a process that will execute the given commands, and returns a file handle to read or write.
pub fn emit_substitution(
    substitute: &Substitute,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    // Create a pipe to get either a writing or reading end
    instructions.emit_code(Opcode::Pipe);
    let jump_to_parent = instructions.emit_jump(Opcode::Fork);
    let commands = match substitute {
        Substitute::In(commands) => {
            // Bound stdout to the writing end of the pipe
            instructions.emit_push_int(1);
            instructions.emit_code(Opcode::Redirect);
            instructions.emit_code(Opcode::Close);
            instructions.emit_code(Opcode::Close);
            commands
        }
        Substitute::Out(commands) => {
            // Bound stdin to the reading end of the pipe
            instructions.emit_code(Opcode::Close);
            instructions.emit_push_int(0);
            instructions.emit_code(Opcode::Redirect);
            instructions.emit_code(Opcode::Close);
            commands
        }
    };
    let last = state.use_values(false);
    emit_already_forked_block(commands, instructions, ctx, cp, locals, state);
    state.use_values(last);

    instructions.patch_jump(jump_to_parent);
    // Do not wait for the child process to finish
    instructions.emit_code(Opcode::PopQWord);

    // Discards the unwanted end of the pipe, and create a string from the other end
    // On Linux, the pipe fd is referenced in the `/proc` filesystem, so we can get the path to the pipe
    if let Substitute::Out(_) = substitute {
        instructions.emit_code(Opcode::Swap);
    }
    instructions.emit_code(Opcode::Close);
    let local = locals.push_value_space(INT_TYPE);
    instructions.emit_set_local(local, INT_TYPE.into(), locals);
    if state.use_values {
        instructions.emit_get_local(local, INT_TYPE.into(), locals);
        instructions.emit_invoke(cp.insert_string("std::process::get_fd_path"));
    }
    // Save the fd to close it later (when the callee has finished)
    state.opened_files.push(local);
}

pub fn emit_subprocess(
    subprocess: &Subprocess,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let jump_to_parent = instructions.emit_jump(Opcode::Fork);
    emit_already_forked(&subprocess.inner, instructions, ctx, cp, locals, state);
    emit_process_end(Some(&subprocess.inner), instructions);
    instructions.patch_jump(jump_to_parent);
    if subprocess.awaited {
        instructions.emit_code(Opcode::Wait);
        if !state.use_values {
            instructions.emit_code(Opcode::PopByte);
        }
    } else if !state.use_values {
        instructions.emit_code(Opcode::PopQWord);
    }
}
