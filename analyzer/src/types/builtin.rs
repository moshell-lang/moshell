use ast::operation::BinaryOperator;

use crate::engine::Engine;

use crate::reef::{Reef, LANG_REEF};
use crate::relations::{LocalId, Relations, SourceId};
use crate::types::ctx::TypeContext;
use crate::types::engine::TypedEngine;
use crate::types::operator::name_operator_method;
use crate::types::ty::{MethodType, Type, TypeId, TypeRef};
use crate::types::{
    Typing, BOOL, ERROR, EXITCODE, FLOAT, GENERIC_OPTION, GENERIC_VECTOR, GLOB, INT, NOTHING, PID,
    STRING, UNIT,
};

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

const LOGICAL_OPERATORS: &[BinaryOperator] = &[BinaryOperator::And, BinaryOperator::Or];

/// Some common types.
pub const STRING_VEC: TypeRef = TypeRef::new(LANG_REEF, TypeId(12));
pub const INT_VEC: TypeRef = TypeRef::new(LANG_REEF, TypeId(13));

/// generic parameters used by the lang reef.
/// The lang reef is a special reef that reuses the same generic parameters for each functions.
pub const GENERIC_PARAMETER_1: TypeRef = TypeRef::new(LANG_REEF, TypeId(14));

/// Adds the native methods to the engine.
fn fill_lang_typed_engine(engine: &mut TypedEngine, typing: &mut Typing) {
    // declare one generic parameter type, methods will reuse it
    let generic_param1 = TypeRef::new(
        LANG_REEF,
        typing.add_type(Type::Polytype, Some("A".to_string())),
    );

    // option containing generic parameter
    let opt_type = typing.add_type(
        Type::Instantiated(GENERIC_OPTION, vec![generic_param1]),
        None,
    );

    engine.add_method(
        EXITCODE.type_id,
        "to_bool",
        MethodType::new(vec![], vec![], BOOL),
    );

    for op in ARITHMETIC_OPERATORS {
        engine.add_method(
            INT.type_id,
            name_operator_method(*op),
            MethodType::new(vec![], vec![INT], INT),
        );
        engine.add_method(
            FLOAT.type_id,
            name_operator_method(*op),
            MethodType::new(vec![], vec![FLOAT], FLOAT),
        );
    }
    engine.add_method(
        INT.type_id,
        name_operator_method(BinaryOperator::Modulo),
        MethodType::new(vec![], vec![INT], INT),
    );
    engine.add_method(BOOL.type_id, "not", MethodType::new(vec![], vec![], BOOL));
    for ty in [BOOL, STRING] {
        for op in EQUALITY_OPERATORS {
            engine.add_method(
                ty.type_id,
                name_operator_method(*op),
                MethodType::new(vec![], vec![ty], BOOL),
            );
        }
    }
    for ty in [INT, FLOAT] {
        for op in COMPARISON_OPERATORS {
            engine.add_method(
                ty.type_id,
                name_operator_method(*op),
                MethodType::new(vec![], vec![ty], BOOL),
            );
        }
    }
    for stringify in [BOOL, EXITCODE, INT, FLOAT] {
        engine.add_method(
            stringify.type_id,
            "to_string",
            MethodType::new(vec![], vec![], STRING),
        );
    }
    engine.add_method(
        INT.type_id,
        "to_float",
        MethodType::new(vec![], vec![], FLOAT),
    );

    engine.add_method(STRING.type_id, "len", MethodType::new(vec![], vec![], INT));
    engine.add_method(
        STRING.type_id,
        name_operator_method(BinaryOperator::Plus),
        MethodType::new(vec![], vec![STRING], STRING),
    );

    engine.add_generic(GENERIC_VECTOR.type_id, generic_param1);

    engine.add_method(
        GENERIC_VECTOR.type_id,
        "[]",
        MethodType::new(vec![], vec![INT], generic_param1),
    );

    engine.add_method(
        GENERIC_VECTOR.type_id,
        "push",
        MethodType::new(vec![], vec![generic_param1], UNIT),
    );

    engine.add_method(
        GENERIC_VECTOR.type_id,
        "pop",
        MethodType::new(vec![], vec![], TypeRef::new(LANG_REEF, opt_type)),
    );
    engine.add_method(
        GENERIC_VECTOR.type_id,
        "len",
        MethodType::new(vec![], vec![], INT),
    );

    engine.add_method(
        STRING.type_id,
        "split",
        MethodType::new(vec![], vec![STRING], STRING_VEC),
    );
    engine.add_method(
        STRING.type_id,
        "bytes",
        MethodType::new(vec![], vec![], INT_VEC),
    );

    for operand in [BOOL, EXITCODE] {
        for op in LOGICAL_OPERATORS {
            engine.add_method(
                operand.type_id,
                name_operator_method(*op),
                MethodType::new(vec![], vec![operand], operand),
            );
        }
    }
    for operand in [INT, FLOAT] {
        engine.add_method(
            operand.type_id,
            "neg",
            MethodType::new(vec![], vec![], operand),
        );
    }

    engine.add_generic(GENERIC_OPTION.type_id, generic_param1);
    engine.add_method(
        GENERIC_OPTION.type_id,
        "is_none",
        MethodType::new(vec![], vec![], BOOL),
    );
    engine.add_method(
        GENERIC_OPTION.type_id,
        "is_some",
        MethodType::new(vec![], vec![], BOOL),
    );
    engine.add_method(
        GENERIC_OPTION.type_id,
        "unwrap",
        MethodType::new(vec![], vec![], generic_param1),
    );
    engine.add_method(
        GENERIC_VECTOR.type_id,
        "[]",
        MethodType::new(vec![], vec![INT, generic_param1], UNIT),
    );

    engine.add_method(
        INT.type_id,
        "to_exitcode",
        MethodType::new(vec![], vec![], EXITCODE),
    );
    engine.add_method(
        EXITCODE.type_id,
        "to_int",
        MethodType::new(vec![], vec![], INT),
    );

    engine.add_method(
        GENERIC_VECTOR.type_id,
        "pop_head",
        MethodType::new(vec![], vec![], TypeRef::new(LANG_REEF, opt_type)),
    );

    engine.add_method(
        GLOB.type_id,
        "spread",
        MethodType::new(vec![], vec![], STRING_VEC),
    );
    engine.add_method(
        PID.type_id,
        "to_string",
        MethodType::new(vec![], vec![], STRING),
    );
}

fn fill_lang_types(typing: &mut Typing) {
    for (primitive, name) in [
        (Type::Error, Some("Error")),
        (Type::Nothing, Some("Nothing")),
        (Type::Unit, Some("Unit")),
        (Type::Bool, Some("Bool")),
        (Type::ExitCode, Some("Exitcode")),
        (Type::Int, Some("Int")),
        (Type::Float, Some("Float")),
        (Type::String, Some("String")),
        (Type::Vector, Some("Vec")),
        (Type::Option, Some("Option")),
        (Type::Glob, Some("Glob")),
        (Type::Pid, Some("Pid")),
        (Type::Instantiated(GENERIC_VECTOR, vec![STRING]), None),
        (Type::Instantiated(GENERIC_VECTOR, vec![INT]), None),
    ] {
        typing.add_type(primitive, name.map(ToString::to_string));
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
    ctx.bind_name("Vec".to_string(), GENERIC_VECTOR.type_id);
    ctx.bind_name("Option".to_string(), GENERIC_OPTION.type_id);
    ctx.bind_name("Glob".to_string(), GLOB.type_id);
    ctx.bind_name("Pid".to_string(), PID.type_id);

    let locals = [
        ERROR,
        NOTHING,
        UNIT,
        BOOL,
        EXITCODE,
        INT,
        FLOAT,
        STRING,
        GENERIC_VECTOR,
        GENERIC_OPTION,
    ];

    ctx.init_locals(SourceId(0), locals.len());

    for (local_id, local) in locals.iter().enumerate() {
        ctx.set_local_typed(SourceId(0), LocalId(local_id), *local);
    }
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
    fill_lang_typed_engine(&mut reef.typed_engine, &mut reef.typing);

    reef
}
