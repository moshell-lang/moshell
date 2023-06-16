use analyzer::types::hir::{Conditional, Loop};
use std::mem::size_of;

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

    emitter.emit_code(Opcode::IfJump);
    let jump_to_then_placeholder = emitter.create_placeholder(size_of::<usize>());
    //jump out of otherwise instructions
    let mut jump_oo_otherwise_placeholder = None;

    if let Some(otherwise) = &conditional.otherwise {
        emit(otherwise, emitter, cp, state);

        // then we jump out of the otherwise instruction
        emitter.emit_code(Opcode::Jump);
        jump_oo_otherwise_placeholder = Some(emitter.create_placeholder(size_of::<usize>()));
    }

    emitter.fill_in_ip(jump_to_then_placeholder, emitter.len());
    emit(&conditional.then, emitter, cp, state);
    if let Some(jump_placeholder) = jump_oo_otherwise_placeholder {
        emitter.fill_in_ip(jump_placeholder, emitter.len());
    }
}

pub fn emit_loop(
    lp: &Loop,
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    let start_ip = emitter.len();

    let mut loop_state = EmissionState::new();
    loop_state.enclosing_loop_start = start_ip;

    if let Some(condition) = &lp.condition {
        emit(condition, emitter, cp, state);
        emitter.emit_code(Opcode::IfNotJump);
        let jump_placeholder = emitter.create_placeholder(size_of::<usize>());
        emit(&lp.body, emitter, cp, &mut loop_state);

        // jump back to loop start
        emitter.emit_code(Opcode::Jump);
        emitter.emit_instruction_pointer(start_ip);

        // if condition is false, jump at end of loop
        emitter.fill_in_ip(jump_placeholder, emitter.len());
    } else {
        emit(&lp.body, emitter, cp, &mut loop_state);
        emitter.emit_code(Opcode::Jump);
        emitter.emit_instruction_pointer(start_ip)
    }

    // fill break placeholders
    let current_ip = emitter.len();
    for placeholder in loop_state.enclosing_loop_end_placeholders {
        emitter.fill_in_ip(placeholder, current_ip)
    }
}

pub fn emit_continue(emitter: &mut Bytecode, state: &mut EmissionState) {
    emitter.emit_code(Opcode::Jump);
    emitter.emit_instruction_pointer(state.enclosing_loop_start);
}

pub fn emit_break(emitter: &mut Bytecode, state: &mut EmissionState) {
    emitter.emit_code(Opcode::Jump);
    state
        .enclosing_loop_end_placeholders
        .push(emitter.create_placeholder(size_of::<usize>()));
}
