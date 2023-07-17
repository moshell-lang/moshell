use analyzer::relations::{Definition, SourceId, Symbol};
use analyzer::types::hir::{Declaration, ExprKind, TypedExpr, TypeId};
use ast::value::LiteralValue;

use crate::Analysis;
use crate::bytecode::{Instructions, Opcode, Placeholder};
use crate::constant_pool::ConstantPool;
use crate::emit::invoke::{
    emit_capture, emit_function_invocation, emit_pipeline, emit_process_call, emit_redirect,
};
use crate::emit::jump::{emit_break, emit_conditional, emit_continue, emit_loop};
use crate::emit::native::emit_natives;
use crate::locals::LocalsLayout;

mod invoke;
mod jump;
mod native;

#[derive(Debug, Clone)]
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

    // the identifier of the current environment being emitted
    pub current_env_id: SourceId,
}

impl EmissionState {

    pub fn new(use_values: bool, current_env_id: SourceId) -> Self {
        Self {
            enclosing_loop_start: 0,
            enclosing_loop_end_placeholders: Vec::new(),
            use_values,
            current_env_id
        }
    }

    /// Create a new emission state for a loop.
    pub fn in_loop(env_id: SourceId, loop_start: u32) -> Self {
        Self {
            enclosing_loop_start: loop_start,
            enclosing_loop_end_placeholders: Vec::new(),
            use_values: false,
            current_env_id: env_id
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
    symbol: Symbol,
    ref_type: TypeId,
    instructions: &mut Instructions,
    locals: &LocalsLayout,
) {
    let size = ref_type.into();
    instructions.emit_get_local(symbol, size, locals);
}

#[allow(clippy::too_many_arguments)]
fn emit_declaration(
    declaration: &Declaration,
    instructions: &mut Instructions,
    analysis: &Analysis,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    if let Some(value) = &declaration.value {
        locals.set_value_space(declaration.identifier, value.ty.into());

        emit_assignment(
            value,
            Symbol::Local(declaration.identifier),
            instructions,
            analysis,
            cp,
            locals,
            state,
        )
    }
}

fn emit_block(
    exprs: &[TypedExpr],
    instructions: &mut Instructions,
    analysis: &Analysis,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    if let Some((last_expr, exprs)) = exprs.split_last() {
        let last_used = state.use_values(false);
        for expr in exprs {
            emit(expr, instructions, analysis, cp, locals, state);
        }
        state.use_values(last_used);
        emit(last_expr, instructions, analysis, cp, locals, state);
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_assignment(
    value: &TypedExpr,
    identifier: Symbol,
    instructions: &mut Instructions,
    analysis: &Analysis,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last = state.use_values(true);

    emit(value, instructions, analysis, cp, locals, state);
    state.use_values(last);

    let returned_value_type = value.ty.into();

    instructions.emit_set_local(identifier, returned_value_type, locals)
}

fn emit_return(
    value: &Option<Box<TypedExpr>>,
    instructions: &mut Instructions,
    analysis: &Analysis,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    if let Some(value) = &value {
        let last_use = state.use_values(true);

        emit(value, instructions, analysis, cp, locals, state);

        state.use_values(last_use);
    }
    instructions.emit_code(Opcode::Return);
}

pub fn emit(
    expr: &TypedExpr,
    instructions: &mut Instructions,
    analysis: &Analysis,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    match &expr.kind {
        ExprKind::Declare(d) => {
            emit_declaration(d, instructions, analysis, cp, locals, state)
        }
        ExprKind::Block(exprs) => {
            emit_block(exprs, instructions,  analysis, cp, locals, state)
        }
        ExprKind::Conditional(c) => {
            emit_conditional(c, instructions, analysis,cp, locals, state)
        }
        ExprKind::ConditionalLoop(l) => {
            emit_loop(l, instructions,  analysis,cp, locals, state)
        }
        ExprKind::Continue => emit_continue(instructions, state),
        ExprKind::Break => emit_break(instructions, state),
        ExprKind::Return(val) => emit_return(val, instructions,  analysis, cp, locals, state),
        ExprKind::Assign(ass) => emit_assignment(
            &ass.rhs,
            ass.identifier,
            instructions,
            analysis,
            cp,
            locals,
            state,
        ),

        ExprKind::Reference(r) => {
            if state.use_values {
                emit_ref(*r, expr.ty, instructions, locals);
            }
        }
        ExprKind::Literal(literal) => {
            if state.use_values {
                emit_literal(literal, instructions, cp);
            }
        }
        ExprKind::FunctionCall(fc) => {
            emit_function_invocation(fc, expr.ty, instructions,  analysis, cp, locals, state)
        }
        ExprKind::ProcessCall(args) => {
            emit_process_call(args, instructions,  analysis, cp, locals, state)
        }
        ExprKind::MethodCall(method) => match method.definition {
            Definition::Native(id) => {
                emit_natives(
                    id,
                    &method.callee,
                    &method.arguments,
                    instructions,
                    analysis,
                    cp,
                    locals,
                    state,
                );
            }
            Definition::User(_) => todo!("invocation of user defined methods"),
        },
        ExprKind::Redirect(redirect) => {
            emit_redirect(redirect, instructions,  analysis, cp, locals, state)
        }
        ExprKind::Pipeline(commands) => {
            emit_pipeline(commands, instructions,  analysis, cp, locals, state)
        }
        ExprKind::Capture(capture) => {
            emit_capture(capture, instructions,  analysis, cp, locals, state);
        }
        _ => unimplemented!(),
    }
}
