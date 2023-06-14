use std::mem::size_of;
use analyzer::types::hir::{Conditional, Loop};

use crate::bytecode::{Bytecode, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::{EmissionState, emit};

pub fn emit_conditional(conditional: &Conditional,
                        emitter: &mut Bytecode,
                        cp: &mut ConstantPool,
                        state: &mut EmissionState) {

    emit(&conditional.condition, emitter, cp, state);

    emitter.emit_code(Opcode::IfJump);
    let if_jump_placeholder = emitter.create_placeholder(size_of::<usize>());
    let mut jump_placeholder = None;

    if let Some(otherwise) = &conditional.otherwise {
        emit(otherwise, emitter, cp, state);

        emitter.emit_code(Opcode::Jump);
        jump_placeholder = Some(emitter.create_placeholder(size_of::<usize>()));
    }

    emitter.fill_in_ip(if_jump_placeholder, emitter.current_frame_ip());
    emit(&conditional.then, emitter, cp, state);
    if let Some(jump_placeholder) = jump_placeholder {
        emitter.fill_in_ip(jump_placeholder, emitter.current_frame_ip());
    }
}

pub fn emit_loop(lp: &Loop,
                 emitter: &mut Bytecode,
                 cp: &mut ConstantPool,
                 state: &mut EmissionState) {
    let start_ip = emitter.current_frame_ip();

    let mut loop_state = EmissionState::new();
    loop_state.last_loop_start = start_ip;

    if let Some(condition) = &lp.condition {
        emit(condition, emitter, cp, state);
        emitter.emit_code(Opcode::IfNotJump);
        let jump_placeholder = emitter.create_placeholder(size_of::<usize>());
        emit(&lp.body, emitter, cp, &mut loop_state);
        emitter.emit_code(Opcode::Jump);
        emitter.emit_instruction_pointer(start_ip);

        emitter.fill_in_ip(jump_placeholder, emitter.current_frame_ip());
    } else {
        emit(&lp.body, emitter, cp, &mut loop_state);
        emitter.emit_code(Opcode::Jump);
        emitter.emit_instruction_pointer(start_ip)
    }

    // fill break placeholders
    let current_ip = emitter.current_frame_ip();
    for placeholder in loop_state.last_loop_end_placeholders {
        emitter.fill_in_ip(placeholder, current_ip)
    }
}

pub fn emit_continue(emitter: &mut Bytecode, state: &mut EmissionState) {
    emitter.emit_code(Opcode::Jump);
    emitter.emit_instruction_pointer(state.last_loop_start);
}

pub fn emit_break(emitter: &mut Bytecode, state: &mut EmissionState) {
    emitter.emit_code(Opcode::Jump);
    state.last_loop_end_placeholders.push(emitter.create_placeholder(size_of::<usize>()));
}