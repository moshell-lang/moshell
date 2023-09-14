use analyzer::types::hir::{Conditional, Loop};

use crate::bytecode::{Instructions, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState, EmitterContext};
use crate::locals::LocalsLayout;

pub fn emit_conditional(
    conditional: &Conditional,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    // emit condition
    let last_uses = state.use_values(true);
    emit(&conditional.condition, instructions, ctx, cp, locals, state);
    state.use_values(last_uses);

    // If the condition is false, go to ELSE.
    let jump_to_else = instructions.emit_jump(Opcode::IfNotJump);
    // Evaluate the if branch.
    emit(&conditional.then, instructions, ctx, cp, locals, state);

    // Go to END.
    let jump_to_end = instructions.emit_jump(Opcode::Jump);

    // ELSE:
    instructions.patch_jump(jump_to_else);
    if let Some(otherwise) = &conditional.otherwise {
        emit(otherwise, instructions, ctx, cp, locals, state);
    }

    // END:
    instructions.patch_jump(jump_to_end);
}

pub fn emit_loop(
    lp: &Loop,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    // START:
    let loop_start = instructions.current_ip();
    let mut loop_state = EmissionState::in_loop(loop_start);

    // loops cannot implicitly return something

    if let Some(condition) = &lp.condition {
        let last_used = state.use_values(true);

        // Evaluate the condition.
        emit(condition, instructions, ctx, cp, locals, state);
        state.use_values(last_used);

        // If the condition is false, go to END.
        let jump_to_end = instructions.emit_jump(Opcode::IfNotJump);
        loop_state.enclosing_loop_end_placeholders.push(jump_to_end);
    }

    loop_state.enclosing_loop_start = loop_start;

    // Evaluate the loop body.
    emit(&lp.body, instructions, ctx, cp, locals, &mut loop_state);
    // Go to START.
    instructions.jump_back_to(loop_start);

    // END:
    for jump_to_end in loop_state.enclosing_loop_end_placeholders {
        instructions.patch_jump(jump_to_end);
    }
}

pub fn emit_continue(instructions: &mut Instructions, state: &EmissionState) {
    instructions.jump_back_to(state.enclosing_loop_start);
}

pub fn emit_break(instructions: &mut Instructions, state: &mut EmissionState) {
    state
        .enclosing_loop_end_placeholders
        .push(instructions.emit_jump(Opcode::Jump));
}
