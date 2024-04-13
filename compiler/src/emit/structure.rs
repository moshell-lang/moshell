use crate::bytecode::Instructions;
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState, EmitterContext};
use crate::locals::LocalsLayout;
use analyzer::hir::{FieldAccess, FieldAssign};

pub fn emit_field_access(
    access: &FieldAccess,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let uses = state.use_values(true);
    emit(&access.object, instructions, ctx, cp, locals, state);
    state.use_values(uses);
    let layout = &ctx.layouts[access.structure.get()];
    instructions.emit_get_field(access.field, layout);
}

pub fn emit_field_assign(
    assign: &FieldAssign,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let value_used = state.use_values(true);
    emit(&assign.object, instructions, ctx, cp, locals, state);
    emit(&assign.new_value, instructions, ctx, cp, locals, state);
    state.use_values(value_used);

    let layout = &ctx.layouts[assign.structure.get()];

    instructions.emit_set_field(assign.field, layout);
}
