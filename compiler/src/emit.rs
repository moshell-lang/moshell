use analyzer::relations::{Definition, Symbol};
use analyzer::types::hir::{Assignment, Declaration, ExprKind, TypedExpr};
use ast::value::LiteralValue;

use crate::bytecode::{Bytecode, Opcode};
use crate::constant_pool::ConstantPool;
use crate::emit::invoke::emit_process_call;
use crate::emit::jump::{emit_break, emit_conditional, emit_continue, emit_loop};
use crate::emit::native::emit_primitive_op;

mod invoke;
mod jump;
mod native;

#[derive(Debug, Clone, Default)]
pub struct EmissionState {
    // the start instruction position of the enclosing loop
    // set to 0 if there is no loop
    pub enclosing_loop_start: usize,
    // All the placeholders waiting for the end of the loop.
    // When the loop compilation ends, all those placeholder are filled with the
    // first instruction pointer after the loop.
    pub enclosing_loop_end_placeholders: Vec<usize>,

    // if set to false, the compiler will avoid emitting literals, var references or will
    // instantly pop values returned from functions, methods and process calls
    // we don't use values by default
    pub use_values: bool
}

impl EmissionState {
    /// Create a new emission state.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new emission state for a loop.
    pub fn in_loop(loop_start: usize) -> Self {
        Self {
            enclosing_loop_start: loop_start,
            enclosing_loop_end_placeholders: Vec::new(),
            use_values: false
        }
    }

    /// sets use_values to given value, and return last value
    pub fn use_values(&mut self, used: bool) -> bool {
        let last_state = self.use_values;
        self.use_values = used;
        last_state
    }
}

fn emit_literal(literal: &LiteralValue, emitter: &mut Bytecode, cp: &mut ConstantPool) {
    match literal {
        LiteralValue::String(string) => {
            let str_ref = cp.insert_string(string.clone());
            emitter.emit_string_constant_ref(str_ref);
        }
        LiteralValue::Int(integer) => {
            emitter.emit_int(*integer);
        }
        LiteralValue::Float(f) => {
            emitter.emit_float(*f);
        }
    }
}

fn emit_ref(symbol: &Symbol, emitter: &mut Bytecode) {
    match symbol {
        Symbol::Local(id) => emitter.emit_get_local(id.0 as u8),
        _ => todo!(),
    }
}

fn emit_declaration(
    declaration: &Declaration,
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    if let Some(value) = &declaration.value {
        state.use_values = true;
        emit(value, emitter, cp, state);
        state.use_values = false;
        emitter.emit_code(Opcode::SetLocal);
        emitter.bytes.push(declaration.identifier.0 as u8);
    }
}

fn emit_block(
    exprs: &[TypedExpr],
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    if let Some((last_expr, exprs)) = exprs.split_last() {
        let used = state.use_values(false);
        for expr in exprs {
            emit(expr, emitter, cp, state);
        }
        state.use_values = used;
        emit(last_expr, emitter, cp, state);
    }
}

fn emit_assignment(
    assignment: &Assignment,
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    emit(&assignment.rhs, emitter, cp, state);
    match assignment.identifier {
        Symbol::Local(id) => emitter.emit_set_local(id.0 as u8),
        Symbol::External(_) => {
            unimplemented!("External variable assignations are not implemented yet")
        }
    }
}

pub fn emit(
    expr: &TypedExpr,
    emitter: &mut Bytecode,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    match &expr.kind {
        ExprKind::Declare(d) => emit_declaration(d, emitter, cp, state),
        ExprKind::Block(exprs) => emit_block(exprs, emitter, cp, state),
        ExprKind::Conditional(c) => emit_conditional(c, emitter, cp, state),
        ExprKind::ConditionalLoop(l) => emit_loop(l, emitter, cp, state),
        ExprKind::Continue => emit_continue(emitter, state),
        ExprKind::Break => emit_break(emitter, state),
        ExprKind::Assign(ass) => emit_assignment(ass, emitter, cp, state),
        ExprKind::Reference(r) => {
            // if the reference's value is not used, then simply do not emit it
            if state.use_values {
                emit_ref(r, emitter)
            }
        }
        ExprKind::Literal(literal) => {
            // if the literal's value is not used, then simply do not emit it
            if state.use_values {
                emit_literal(literal, emitter, cp)
            }
        }
        ExprKind::ProcessCall(args) => emit_process_call(args, emitter, cp, state),
        ExprKind::MethodCall(method) => match method.definition {
            Definition::Native(id) => {
                emit_primitive_op(id, &method.callee, &method.arguments, emitter, cp, state);
            }
            Definition::User(_) => todo!("user defined method"),
        },
        _ => {}
    }
}
