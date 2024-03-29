use crate::bytecode::{Instructions, Opcode};
use crate::constant_pool::ConstantPool;
use crate::context::EmitterContext;
use crate::emit::native::{STRING_INDEX, STRING_LEN, VEC_INDEX, VEC_LEN};
use crate::emit::{emit, EmissionState};
use crate::locals::LocalsLayout;
use crate::r#type::ValueStackSize;
use analyzer::reef::ReefId;
use analyzer::relations::LocalId;
use analyzer::types::builtin::STRING_STRUCT;
use analyzer::types::engine::StructureId;
use analyzer::types::hir::{ForKind, ForLoop, RangeFor, TypedExpr};
use analyzer::types::ty::Type;
use analyzer::types::{GENERIC_VECTOR, INT, STRING};

pub(super) fn emit_for_loop(
    it: &ForLoop,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    match it.kind.as_ref() {
        ForKind::Range(range) => {
            let type_ref = range.iterable.ty;
            let iterable_type = ctx.get_type(type_ref);
            match iterable_type {
                Type::Instantiated(vec, params) if *vec == GENERIC_VECTOR => {
                    let param = params[0];
                    emit_for_iterable(
                        range,
                        &it.body,
                        |_, instructions, _, _| {
                            instructions.emit_push_int(0);
                        },
                        |instructions, cp| {
                            instructions.emit_invoke(cp.insert_string(VEC_INDEX));
                            if !param.is_obj() {
                                instructions.emit_code(Opcode::Unbox);
                            }
                        },
                        |instructions, cp| {
                            instructions.emit_invoke(cp.insert_string(VEC_LEN));
                        },
                        Opcode::IntLessThan,
                        |_, instructions, _, _| {
                            instructions.emit_push_int(1);
                        },
                        instructions,
                        ctx,
                        cp,
                        locals,
                        state,
                    );
                }
                Type::Structure(_, string) if *string == STRING_STRUCT => {
                    emit_for_iterable(
                        range,
                        &it.body,
                        |_, instructions, _, _| {
                            instructions.emit_push_int(0);
                        },
                        |instructions, cp| {
                            instructions.emit_invoke(cp.insert_string(STRING_INDEX));
                        },
                        |instructions, cp| {
                            instructions.emit_invoke(cp.insert_string(STRING_LEN));
                        },
                        Opcode::IntLessThan,
                        |_, instructions, cp, locals| {
                            instructions.emit_get_local(range.receiver, STRING.into(), locals);
                            instructions.emit_invoke(cp.insert_string(STRING_LEN));
                        },
                        instructions,
                        ctx,
                        cp,
                        locals,
                        state,
                    );
                }
                Type::Structure(_, structure_id) => {
                    // Int range
                    let layout = ctx.get_layout(ReefId(1), *structure_id);
                    emit_for_iterable(
                        range,
                        &it.body,
                        |iterator_id, instructions, _, locals| {
                            // Emit start
                            instructions.emit_get_local(iterator_id, type_ref.into(), locals);
                            instructions.emit_get_field(LocalId(0), layout);
                        },
                        |instructions, _| {
                            instructions.emit_code(Opcode::Swap);
                            instructions.emit_pop(ValueStackSize::QWord);
                        },
                        |instructions, _| {
                            instructions.emit_get_field(LocalId(1), layout);
                        },
                        if *structure_id == StructureId(0) {
                            Opcode::IntLessThan
                        } else {
                            Opcode::IntLessOrEqual
                        },
                        |iterator_id, instructions, _, locals| {
                            instructions.emit_get_local(iterator_id, type_ref.into(), locals);
                            instructions.emit_get_field(LocalId(2), layout);
                        },
                        instructions,
                        ctx,
                        cp,
                        locals,
                        state,
                    );
                }
                _ => panic!("Unexpected iterable {iterable_type:?} type"),
            }
        }
        ForKind::Conditional(cond) => {
            emit(&cond.initializer, instructions, ctx, cp, locals, state);

            let loop_start = instructions.current_ip();
            let mut loop_state = EmissionState::in_loop();

            let last_used = state.use_values(true);

            // Evaluate the condition.
            emit(&cond.condition, instructions, ctx, cp, locals, state);
            state.use_values(last_used);

            // If the condition is false, go to END.
            let jump_to_end = instructions.emit_jump(Opcode::IfNotJump);
            loop_state.enclosing_loop_end_placeholders.push(jump_to_end);

            // Evaluate the loop body.
            emit(&it.body, instructions, ctx, cp, locals, &mut loop_state);
            for jump_to_increment in loop_state.enclosing_loop_start_placeholders {
                instructions.patch_jump(jump_to_increment);
            }
            emit(&cond.increment, instructions, ctx, cp, locals, state);
            // Go to START.
            instructions.jump_back_to(loop_start);

            // END:
            for jump_to_end in loop_state.enclosing_loop_end_placeholders {
                instructions.patch_jump(jump_to_end);
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_for_iterable<
    V: FnOnce(LocalId, &mut Instructions, &mut ConstantPool, &mut LocalsLayout),
    F: FnOnce(&mut Instructions, &mut ConstantPool),
    L: FnOnce(&mut Instructions, &mut ConstantPool),
    I: FnOnce(LocalId, &mut Instructions, &mut ConstantPool, &mut LocalsLayout),
>(
    RangeFor {
        receiver,
        receiver_type,
        iterable,
    }: &RangeFor,
    body: &TypedExpr,
    initial_value: V,
    indexer: F,
    len: L,
    comparator: Opcode,
    increment: I,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let iterator_id = locals.push_value_space(iterable.ty);
    let last_used = state.use_values(true);
    emit(iterable, instructions, ctx, cp, locals, state);
    state.use_values(last_used);
    instructions.emit_set_local(iterator_id, iterable.ty.into(), locals);

    let index_id = locals.push_value_space(INT);
    initial_value(iterator_id, instructions, cp, locals);
    instructions.emit_set_local(index_id, INT.into(), locals);

    let loop_start = instructions.current_ip();
    let mut loop_state = EmissionState::in_loop();
    instructions.emit_get_local(index_id, INT.into(), locals);
    instructions.emit_get_local(iterator_id, iterable.ty.into(), locals);
    len(instructions, cp);
    instructions.emit_code(comparator);
    let jump_to_end = instructions.emit_jump(Opcode::IfNotJump);
    loop_state.enclosing_loop_end_placeholders.push(jump_to_end);

    // Indexes the iterable and stores the result in the receiver.
    locals.set_value_space(*receiver, *receiver_type);
    instructions.emit_get_local(iterator_id, iterable.ty.into(), locals);
    instructions.emit_get_local(index_id, INT.into(), locals);
    indexer(instructions, cp);
    instructions.emit_set_local(*receiver, (*receiver_type).into(), locals);

    emit(body, instructions, ctx, cp, locals, &mut loop_state);
    for jump_to_increment in loop_state.enclosing_loop_start_placeholders {
        instructions.patch_jump(jump_to_increment);
    }
    instructions.emit_get_local(index_id, INT.into(), locals);
    increment(iterator_id, instructions, cp, locals);
    instructions.emit_code(Opcode::IntAdd);
    instructions.emit_set_local(index_id, INT.into(), locals);

    instructions.jump_back_to(loop_start);
    for jump_to_end in loop_state.enclosing_loop_end_placeholders {
        instructions.patch_jump(jump_to_end);
    }
}
