use analyzer::engine::Engine;
use analyzer::types::hir::{Conditional, Loop};
use analyzer::types::Typing;

use crate::bytecode::{Instructions, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState};

pub fn emit_conditional(
    conditional: &Conditional,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    // emit condition
    let last = state.use_values(true);
    emit(
        &conditional.condition,
        instructions,
        typing,
        engine,
        cp,
        state,
    );
    state.use_values(last);

    // If the condition is false, go to ELSE.
    let jump_to_else = instructions.emit_jump(Opcode::IfNotJump);
    // Evaluate the if branch.
    emit(&conditional.then, instructions, typing, engine, cp, state);

    // Go to END.
    let jump_to_end = instructions.emit_jump(Opcode::Jump);

    // ELSE:
    instructions.patch_jump(jump_to_else);
    if let Some(otherwise) = &conditional.otherwise {
        emit(otherwise, instructions, typing, engine, cp, state);
    }

    // END:
    instructions.patch_jump(jump_to_end);
}

pub fn emit_loop(
    lp: &Loop,
    emitter: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    // START:
    let loop_start = emitter.current_ip();
    let mut loop_state = EmissionState::in_loop(loop_start);

    if let Some(condition) = &lp.condition {
        let last = state.use_values(true);
        // Evaluate the condition.
        emit(condition, emitter, typing, engine, cp, state);
        state.use_values(last);

        // If the condition is false, go to END.
        let jump_to_end = emitter.emit_jump(Opcode::IfNotJump);
        loop_state.enclosing_loop_end_placeholders.push(jump_to_end);
    }

    loop_state.enclosing_loop_start = loop_start;

    // Evaluate the loop body.
    emit(&lp.body, emitter, typing, engine, cp, &mut loop_state);
    // Go to START.
    emitter.jump_back_to(loop_start);

    // END:
    for jump_to_end in loop_state.enclosing_loop_end_placeholders {
        emitter.patch_jump(jump_to_end);
    }
}

pub fn emit_continue(emitter: &mut Instructions, state: &mut EmissionState) {
    emitter.jump_back_to(state.enclosing_loop_start);
}

pub fn emit_break(emitter: &mut Instructions, state: &mut EmissionState) {
    state
        .enclosing_loop_end_placeholders
        .push(emitter.emit_jump(Opcode::Jump));
}
