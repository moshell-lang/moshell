use analyzer::types::hir::{Conditional, Loop};

use crate::bytecode::{Bytecode, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState};

pub fn emit_conditional(
    conditional: &Conditional,
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    emit(&conditional.condition, emitter, cp, state);

    // If the condition is false, go to ELSE.
    let jump_to_else = emitter.emit_jump(Opcode::IfNotJump);
    // Evaluate the if branch.
    emit(&conditional.then, emitter, cp, state);

    // Go to END.
    let jump_to_end = emitter.emit_jump(Opcode::Jump);

    // ELSE:
    emitter.patch_jump(jump_to_else);
    if let Some(otherwise) = &conditional.otherwise {
        emit(otherwise, emitter, cp, state);
    }

    // END:
    emitter.patch_jump(jump_to_end);
}

pub fn emit_loop(
    lp: &Loop,
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    // START:
    let loop_start = emitter.current_ip();
    let mut loop_state = EmissionState::in_loop(loop_start);

    if let Some(condition) = &lp.condition {
        // Evaluate the condition.
        emit(condition, emitter, cp, state);
        // If the condition is false, go to END.
        let jump_to_end = emitter.emit_jump(Opcode::IfNotJump);
        loop_state.enclosing_loop_end_placeholders.push(jump_to_end);
    }

    loop_state.enclosing_loop_start = loop_start;

    // Evaluate the loop body.
    emit(&lp.body, emitter, cp, &mut loop_state);
    // Go to START.
    emitter.jump_back_to(loop_start);
    // END:
    for jump_to_end in &loop_state.enclosing_loop_end_placeholders {
        emitter.patch_jump(*jump_to_end);
    }
}

pub fn emit_continue(emitter: &mut Bytecode, state: &mut EmissionState) {
    emitter.emit_code(Opcode::Jump);
    emitter.emit_instruction_pointer(state.enclosing_loop_start);
}

pub fn emit_break(emitter: &mut Bytecode, state: &mut EmissionState) {
    state
        .enclosing_loop_end_placeholders
        .push(emitter.emit_jump(Opcode::Jump));
}
