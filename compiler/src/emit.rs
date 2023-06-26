use analyzer::engine::Engine;
use analyzer::relations::{Definition, Symbol};

use analyzer::types::hir::{Declaration, ExprKind, TypeId, TypedExpr};
use analyzer::types::*;
use ast::value::LiteralValue;

use crate::bytecode::{Instructions, Opcode, Placeholder};
use crate::constant_pool::ConstantPool;
use crate::emit::invoke::{emit_function_call, emit_process_call};
use crate::emit::jump::{emit_break, emit_conditional, emit_continue, emit_loop};
use crate::emit::native::emit_primitive_op;

mod invoke;
mod jump;
mod native;

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
    }
}

fn emit_ref(symbol: &Symbol, ref_type: TypeId, instructions: &mut Instructions) {
    match symbol {
        Symbol::Local(id) => instructions.emit_get_local(id.0 as u32, ref_type),
        _ => todo!(),
    }
}

fn emit_declaration(
    declaration: &Declaration,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    if let Some(value) = &declaration.value {
        emit_assignment(
            value,
            Symbol::Local(declaration.identifier),
            instructions,
            typing,
            engine,
            cp,
            state,
        )
    }
}

fn emit_block(
    exprs: &Vec<TypedExpr>,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    if let Some((last_expr, exprs)) = exprs.split_last() {
        let used = state.use_values(false);
        for expr in exprs {
            emit(expr, instructions, typing, engine, cp, state);
        }
        state.use_values(used);
        emit(last_expr, instructions, typing, engine, cp, state);
    }
}

fn emit_assignment(
    value: &TypedExpr,
    identifier: Symbol,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    let last = state.use_values(true);
    emit(&value, instructions, typing, engine, cp, state);
    state.use_values(last);

    match identifier {
        Symbol::Local(id) => instructions.emit_set_local(id.0 as u32, value.ty),
        Symbol::External(_) => {
            unimplemented!("External variable assignations are not implemented yet")
        }
    }
}

fn emit_return(
    value: &Option<Box<TypedExpr>>,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    if let Some(value) = &value {
        let last = state.use_values(true);
        emit(value, instructions, typing, engine, cp, state);
        state.use_values(last);
    }
    instructions.emit_code(Opcode::Return);
}

pub fn emit(
    expr: &TypedExpr,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    state: &mut EmissionState,
) {
    match &expr.kind {
        ExprKind::Declare(d) => emit_declaration(d, instructions, typing, engine, cp, state),
        ExprKind::Block(exprs) => emit_block(exprs, instructions, typing, engine, cp, state),
        ExprKind::Conditional(c) => emit_conditional(c, instructions, typing, engine, cp, state),
        ExprKind::ConditionalLoop(l) => emit_loop(l, instructions, typing, engine, cp, state),
        ExprKind::Continue => emit_continue(instructions, state),
        ExprKind::Break => emit_break(instructions, state),
        ExprKind::Return(val) => emit_return(val, instructions, typing, engine, cp, state),
        ExprKind::Assign(ass) => emit_assignment(
            &ass.rhs,
            ass.identifier,
            instructions,
            typing,
            engine,
            cp,
            state,
        ),
        ExprKind::Reference(r) => {
            if state.use_values {
                emit_ref(r, expr.ty, instructions);
            }
        }
        ExprKind::Literal(literal) => {
            if state.use_values {
                emit_literal(literal, instructions, cp);
            }
        }
        ExprKind::FunctionCall(fc) => {
            emit_function_call(fc, expr.ty, instructions, typing, engine, cp, state)
        }
        ExprKind::ProcessCall(args) => {
            emit_process_call(args, instructions, typing, engine, cp, state)
        }
        ExprKind::MethodCall(method) => match method.definition {
            Definition::Native(id) => {
                emit_primitive_op(
                    id,
                    &method.callee,
                    &method.arguments,
                    instructions,
                    typing,
                    engine,
                    cp,
                    state,
                );
            }
            Definition::User(_) => todo!("invocation of user defined methods"),
        },
        _ => unimplemented!(),
    }
}
