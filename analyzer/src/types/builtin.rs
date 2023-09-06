use ast::operation::BinaryOperator;

use crate::engine::Engine;
use crate::reef::Reef;
use crate::relations::{NativeId, Relations};
use crate::types::ctx::TypeContext;
use crate::types::engine::TypedEngine;
use crate::types::operator::name_operator_method;
use crate::types::ty::{MethodType, Type};
use crate::types::{Typing, BOOL, EXITCODE, FLOAT, INT, NOTHING, STRING, UNIT};

const ARITHMETIC_OPERATORS: &[BinaryOperator] = &[
    BinaryOperator::Plus,
    BinaryOperator::Minus,
    BinaryOperator::Times,
    BinaryOperator::Divide,
];
const COMPARISON_OPERATORS: &[BinaryOperator] = &[
    BinaryOperator::EqualEqual,
    BinaryOperator::NotEqual,
    BinaryOperator::Less,
    BinaryOperator::LessEqual,
    BinaryOperator::Greater,
    BinaryOperator::GreaterEqual,
];
const EQUALITY_OPERATORS: &[BinaryOperator] =
    &[BinaryOperator::EqualEqual, BinaryOperator::NotEqual];

/// A sequential generator for native object ids.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
struct VariableGenerator(usize);

impl VariableGenerator {
    /// Gets and increments the next native object id.
    fn next(&mut self) -> NativeId {
        let id = self.0;
        self.0 += 1;
        NativeId(id)
    }
}

/// Adds the native methods to the engine.
fn fill_lang_typed_engine(engine: &mut TypedEngine) {
    let mut gen = VariableGenerator::default();
    engine.add_method(
        EXITCODE.type_id,
        "to_bool",
        MethodType::native(vec![], BOOL, gen.next()),
    );
    for op in ARITHMETIC_OPERATORS {
        engine.add_method(
            INT.type_id,
            name_operator_method(*op),
            MethodType::native(vec![INT], INT, gen.next()),
        );
        engine.add_method(
            FLOAT.type_id,
            name_operator_method(*op),
            MethodType::native(vec![FLOAT], FLOAT, gen.next()),
        );
    }
    engine.add_method(
        INT.type_id,
        name_operator_method(BinaryOperator::Modulo),
        MethodType::native(vec![INT], INT, gen.next()),
    );
    engine.add_method(
        BOOL.type_id,
        "not",
        MethodType::native(vec![], BOOL, gen.next()),
    );
    for ty in [BOOL, STRING] {
        for op in EQUALITY_OPERATORS {
            engine.add_method(
                ty.type_id,
                name_operator_method(*op),
                MethodType::native(vec![ty], BOOL, gen.next()),
            );
        }
    }
    for ty in [INT, FLOAT] {
        for op in COMPARISON_OPERATORS {
            engine.add_method(
                ty.type_id,
                name_operator_method(*op),
                MethodType::native(vec![ty], BOOL, gen.next()),
            );
        }
    }
    for stringify in [BOOL, EXITCODE, INT, FLOAT] {
        engine.add_method(
            stringify.type_id,
            "to_string",
            MethodType::native(vec![], STRING, gen.next()),
        );
    }
    engine.add_method(
        INT.type_id,
        "to_float",
        MethodType::native(vec![], FLOAT, gen.next()),
    );
    engine.add_method(
        STRING.type_id,
        "len",
        MethodType::native(vec![], INT, gen.next()),
    );
    engine.add_method(
        STRING.type_id,
        name_operator_method(BinaryOperator::Plus),
        MethodType::native(vec![STRING], STRING, gen.next()),
    );
}

fn fill_lang_types(typing: &mut Typing) {
    for primitive in [
        Type::Error,
        Type::Nothing,
        Type::Unit,
        Type::Bool,
        Type::ExitCode,
        Type::Int,
        Type::Float,
        Type::String,
    ] {
        typing.add_type(primitive);
    }
    typing.set_implicit_conversion(EXITCODE.type_id, BOOL);
    typing.set_implicit_conversion(INT.type_id, FLOAT);
}

fn fill_lang_bindings(ctx: &mut TypeContext) {
    ctx.bind_name("Nothing".to_string(), NOTHING.type_id);
    ctx.bind_name("Unit".to_string(), UNIT.type_id);
    ctx.bind_name("Bool".to_string(), BOOL.type_id);
    ctx.bind_name("Exitcode".to_string(), EXITCODE.type_id);
    ctx.bind_name("Int".to_string(), INT.type_id);
    ctx.bind_name("Float".to_string(), FLOAT.type_id);
    ctx.bind_name("String".to_string(), STRING.type_id);
}

pub fn lang_reef() -> Reef<'static> {
    let mut reef = Reef {
        name: "lang".to_string(),
        engine: Engine::default(),
        relations: Relations::default(),
        typed_engine: TypedEngine::default(),
        typing: Typing::default(),
        type_context: TypeContext::default(),
    };

    fill_lang_types(&mut reef.typing);
    fill_lang_bindings(&mut reef.type_context);
    fill_lang_typed_engine(&mut reef.typed_engine);

    reef
}
