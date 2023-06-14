use std::mem::size_of;
use analyzer::types::hir::Conditional;

use crate::bytecode::{Bytecode, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::emit;

pub fn emit_conditional(conditional: &Conditional, emitter: &mut Bytecode, cp: &mut ConstantPool) {
    emit(&conditional.condition, emitter, cp);

    let if_jump_placeholder = emitter.emit_code_backpatched(Opcode::IfJump, size_of::<usize>());
    let mut jump_placeholder = None;
    if let Some(otherwise) = &conditional.otherwise {
        emit(otherwise, emitter, cp);
        jump_placeholder = Some(emitter.emit_code_backpatched(Opcode::Jump, size_of::<usize>()));
    }

    emitter.fill_in_ip(if_jump_placeholder, emitter.current_frame_pos());
    emit(&conditional.then, emitter, cp);
    if let Some(jump_placeholder) = jump_placeholder {
        emitter.fill_in_ip(jump_placeholder, emitter.current_frame_pos());
    }
}