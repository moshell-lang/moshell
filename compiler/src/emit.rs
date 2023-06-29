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
use crate::locals::LocalsLayout;
use crate::r#type::ValueStackSize;

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

    // if set to true, the compiler will place the value it usually pushes in operand stack in first local's index, regardless
    // of the LocalsLayout.
    // This is how the function returns their values according to bytecode specifications
    pub is_returning_value: bool,
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

    /// sets use_values to given value, and return last value
    pub fn returning_value(&mut self, returning: bool) -> bool {
        let last_state = self.is_returning_value;
        self.is_returning_value = returning;
        last_state
    }
}

fn emit_literal(
    literal: &LiteralValue,
    instructions: &mut Instructions,
    cp: &mut ConstantPool,
    returned: bool,
) {
    let size = match literal {
        LiteralValue::String(string) => {
            let str_ref = cp.insert_string(string);
            instructions.emit_push_constant_ref(str_ref);
            ValueStackSize::Reference
        }
        LiteralValue::Int(integer) => {
            instructions.emit_push_int(*integer);
            ValueStackSize::QWord
        }
        LiteralValue::Float(f) => {
            instructions.emit_push_float(*f);
            ValueStackSize::QWord
        }
        LiteralValue::Bool(b) => {
            instructions.emit_push_byte(*b as u8);
            ValueStackSize::Byte
        }
    };

    if returned {
        instructions.emit_set_return(size);
    }
}

fn emit_ref(
    symbol: &Symbol,
    ref_type: TypeId,
    instructions: &mut Instructions,
    locals: &LocalsLayout,
) {
    match symbol {
        Symbol::Local(id) => instructions.emit_get_local(*id, ref_type.into(), locals),

        _ => todo!(),
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_declaration(
    declaration: &Declaration,
    tpe: TypeId,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    if let Some(value) = &declaration.value {
        locals.expand_layout(declaration.identifier, tpe);

        emit_assignment(
            value,
            Symbol::Local(declaration.identifier),
            instructions,
            typing,
            engine,
            cp,
            locals,
            state,
        )
    }
}

fn emit_block(
    exprs: &[TypedExpr],
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    if let Some((last_expr, exprs)) = exprs.split_last() {
        let last_used = state.use_values(false);
        let last_returns = state.returning_value(false);
        for expr in exprs {
            emit(expr, instructions, typing, engine, cp, locals, state);
        }
        state.use_values(last_used);
        state.returning_value(last_returns);
        emit(last_expr, instructions, typing, engine, cp, locals, state);
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_assignment(
    value: &TypedExpr,
    identifier: Symbol,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    let last = state.use_values(true);

    emit(value, instructions, typing, engine, cp, locals, state);
    state.use_values(last);

    let returned_value_type = value.ty.into();

    match identifier {
        Symbol::Local(id) => {
            if state.is_returning_value {
                instructions.emit_set_return(returned_value_type)
            } else {
                instructions.emit_set_local(id, returned_value_type, locals)
            }
        }

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
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    if let Some(value) = &value {
        let last_use = state.use_values(true);
        let last_return = state.returning_value(true);

        emit(value, instructions, typing, engine, cp, locals, state);

        state.use_values(last_use);
        state.returning_value(last_return);
    }
    instructions.emit_code(Opcode::Return);
}

pub fn emit(
    expr: &TypedExpr,
    instructions: &mut Instructions,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
    locals: &mut LocalsLayout,
    state: &mut EmissionState,
) {
    match &expr.kind {
        ExprKind::Declare(d) => {
            emit_declaration(d, expr.ty, instructions, typing, engine, cp, locals, state)
        }
        ExprKind::Block(exprs) => {
            emit_block(exprs, instructions, typing, engine, cp, locals, state)
        }
        ExprKind::Conditional(c) => {
            emit_conditional(c, instructions, typing, engine, cp, locals, state)
        }
        ExprKind::ConditionalLoop(l) => {
            emit_loop(l, instructions, typing, engine, cp, locals, state)
        }
        ExprKind::Continue => emit_continue(instructions, state),
        ExprKind::Break => emit_break(instructions, state),
        ExprKind::Return(val) => emit_return(val, instructions, typing, engine, cp, locals, state),
        ExprKind::Assign(ass) => emit_assignment(
            &ass.rhs,
            ass.identifier,
            instructions,
            typing,
            engine,
            cp,
            locals,
            state,
        ),

        ExprKind::Reference(r) => {
            if state.use_values {
                emit_ref(r, expr.ty, instructions, locals);
            }
        }
        ExprKind::Literal(literal) => {
            if state.use_values {
                emit_literal(literal, instructions, cp, state.is_returning_value);
            }
        }
        ExprKind::FunctionCall(fc) => {
            emit_function_call(fc, expr.ty, instructions, typing, engine, cp, locals, state)
        }
        ExprKind::ProcessCall(args) => {
            emit_process_call(args, instructions, typing, engine, cp, locals, state)
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
                    locals,
                    state,
                );
            }
            Definition::User(_) => todo!("invocation of user defined methods"),
        },
        _ => unimplemented!(),
    }
}
