use analyzer::types::hir::{Declaration, ExprKind, TypedExpr, Var};
use analyzer::types::ty::TypeRef;
use ast::value::LiteralValue;

use crate::bytecode::{Instructions, Opcode, Placeholder};
use crate::constant_pool::ConstantPool;
use crate::context::EmitterContext;
use crate::emit::identifier::{expose_variable, Identifier};
use crate::emit::invoke::{
    emit_capture, emit_function_invocation, emit_pipeline, emit_process_call, emit_redirect,
    emit_subprocess,
};
use crate::emit::jump::{emit_break, emit_conditional, emit_continue, emit_loop};
use crate::emit::native::emit_natives;
use crate::emit::structure::{emit_field_access, emit_field_assign};
use crate::locals::LocalsLayout;

mod identifier;
mod invoke;
mod jump;
mod native;
mod structure;

#[derive(Debug, Clone, Default)]
pub struct EmissionState {
    // the start instruction position of the enclosing loop
    // set to 0 if there is no loop
    pub enclosing_loop_start: u32,

    // All the placeholders waiting for the end of the loop.
    // When the loop compilation ends, all those placeholder are filled with the
    // first instruction pointer after the loop.
    pub enclosing_loop_end_placeholders: Vec<Placeholder>,

    // if set to false, the compiler will avoid emitting literals, var references or will
    // instantly pop values returned from functions, methods and process calls
    pub use_values: bool,
}

impl EmissionState {
    /// Create a new emission state for a loop.
    pub fn in_loop(loop_start: u32) -> Self {
        Self {
            enclosing_loop_start: loop_start,
            ..Self::default()
        }
    }

    /// sets use_values to given value, and return last value
    pub fn use_values(&mut self, used: bool) -> bool {
        let last_state = self.use_values;
        self.use_values = used;
        last_state
    }
}

fn emit_literal(literal: &LiteralValue, instructions: &mut Instructions, cp: &mut ConstantPool) {
    match literal {
        LiteralValue::String(string) => {
            let str_ref = cp.insert_string(string);
            instructions.emit_push_constant_ref(str_ref);
        }
        LiteralValue::Int(integer) => {
            instructions.emit_push_int(*integer);
        }
        LiteralValue::Float(f) => {
            instructions.emit_push_float(*f);
        }
        LiteralValue::Bool(b) => {
            instructions.emit_push_byte(*b as u8);
        }
    };
}

fn emit_ref(
    var: Var,
    ctx: &EmitterContext,
    ref_type: TypeRef,
    instructions: &mut Instructions,
    cp: &mut ConstantPool,
    locals: &LocalsLayout,
) {
    let size = ref_type.into();
    match expose_variable(ctx, var, cp) {
        Identifier::Local(id) => {
            instructions.emit_get_local(id, size, locals);
        }
        Identifier::Capture(id) => {
            instructions.emit_get_capture(id, size, locals);
        }
        Identifier::External(id) => {
            instructions.emit_get_external(id, size);
        }
    }
}

fn emit_declaration(
    declaration: &Declaration,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let variable = ctx
        .environment
        .symbols
        .get(declaration.identifier)
        .expect("The declared variable should be in the current environment.");

    if let Some(value) = &declaration.value {
        locals.set_value_space(declaration.identifier, value.ty);

        if variable.is_exported() && ctx.environment.is_script {
            let offset = locals
                .get_index(declaration.identifier)
                .expect("Variable just have been declared");
            cp.insert_exported(&variable.name, offset, value.ty.is_obj());
        }

        emit_assignment(
            value,
            Var::Local(declaration.identifier),
            instructions,
            ctx,
            cp,
            locals,
            state,
        );
    }
}

fn emit_block(
    exprs: &[TypedExpr],
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    if let Some((last_expr, exprs)) = exprs.split_last() {
        let last_used = state.use_values(false);
        for expr in exprs {
            emit(expr, instructions, ctx, cp, locals, state);
        }
        state.use_values(last_used);
        emit(last_expr, instructions, ctx, cp, locals, state);
    }
}

fn emit_assignment(
    value: &TypedExpr,
    var: Var,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last = state.use_values(true);

    emit(value, instructions, ctx, cp, locals, state);
    state.use_values(last);

    let returned_value_type = value.ty.into();

    match expose_variable(ctx, var, cp) {
        Identifier::Local(id) => {
            instructions.emit_set_local(id, returned_value_type, locals);
        }
        Identifier::Capture(id) => {
            instructions.emit_set_capture(id, returned_value_type, locals);
        }
        Identifier::External(id) => {
            instructions.emit_set_external(id, returned_value_type);
        }
    }
}

fn emit_return(
    value: &Option<Box<TypedExpr>>,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    if let Some(value) = &value {
        let last_use = state.use_values(true);

        emit(value, instructions, ctx, cp, locals, state);

        state.use_values(last_use);
    }
    instructions.emit_code(Opcode::Return);
}

pub fn emit(
    expr: &TypedExpr,
    instructions: &mut Instructions,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    instructions.push_position(expr.segment.start);
    match &expr.kind {
        ExprKind::Declare(d) => {
            emit_declaration(d, instructions, ctx, cp, locals, state);
        }
        ExprKind::Block(exprs) => {
            emit_block(exprs, instructions, ctx, cp, locals, state);
        }
        ExprKind::Conditional(c) => emit_conditional(c, instructions, ctx, cp, locals, state),
        ExprKind::ConditionalLoop(l) => emit_loop(l, instructions, ctx, cp, locals, state),
        ExprKind::Continue => emit_continue(instructions, state),
        ExprKind::Break => emit_break(instructions, state),
        ExprKind::Return(val) => emit_return(val, instructions, ctx, cp, locals, state),
        ExprKind::LocalAssign(ass) => emit_assignment(
            &ass.rhs,
            ass.identifier,
            instructions,
            ctx,
            cp,
            locals,
            state,
        ),
        ExprKind::FieldAccess(access) => {
            emit_field_access(access, instructions, ctx, cp, locals, state)
        }
        ExprKind::FieldAssign(assign) => {
            emit_field_assign(assign, instructions, ctx, cp, locals, state)
        }
        ExprKind::Reference(symbol) => {
            if state.use_values {
                emit_ref(*symbol, ctx, expr.ty, instructions, cp, locals);
            }
        }
        ExprKind::Literal(literal) => {
            if state.use_values {
                emit_literal(literal, instructions, cp);
            }
        }
        ExprKind::FunctionCall(fc) => {
            emit_function_invocation(fc, expr.ty, instructions, ctx, cp, locals, state)
        }
        ExprKind::ProcessCall(args) => {
            emit_process_call(args, instructions, ctx, cp, locals, state)
        }
        ExprKind::MethodCall(method) => emit_natives(
            method.function_id,
            method,
            expr.ty,
            instructions,
            ctx,
            cp,
            locals,
            state,
        ),
        ExprKind::Redirect(redirect) => {
            emit_redirect(redirect, instructions, ctx, cp, locals, state)
        }
        ExprKind::Pipeline(commands) => {
            emit_pipeline(commands, instructions, ctx, cp, locals, state)
        }
        ExprKind::Capture(capture) => {
            emit_capture(capture, instructions, ctx, cp, locals, state);
        }
        ExprKind::Subprocess(subprocess) => {
            emit_subprocess(subprocess, instructions, ctx, cp, locals, state)
        }
        ExprKind::Noop => {}
        ExprKind::Convert(_) => unimplemented!(),
    }
    instructions.push_position(expr.segment.start)
}
