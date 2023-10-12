use ast::operation::BinaryOperator;

use crate::engine::Engine;
use crate::reef::{Reef, LANG_REEF};
use crate::relations::{NativeId, Relations, SourceId};
use crate::types;
use crate::types::ctx::TypeContext;
use crate::types::engine::TypedEngine;
use crate::types::operator::name_operator_method;
use crate::types::ty::{MethodType, Type, TypeId, TypeRef};
use crate::types::{
    Typing, BOOL, ERROR, EXITCODE, FLOAT, GENERIC_OPTION, GENERIC_VECTOR, INT, NOTHING, STRING,
    UNIT,
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
fn fill_lang_typed_engine(engine: &mut TypedEngine, typing: &mut Typing) {
    let mut gen = VariableGenerator::default();

    // declare one generic parameter type, methods will reuse it
    let generic_param1 = TypeRef::new(
        LANG_REEF,
        typing.add_type(Type::Polytype, Some("A".to_string())),
    );

    // option containing generic parameter
    let opt_type = typing.add_type(
        Type::Instantiated(types::GENERIC_OPTION, vec![generic_param1]),
        None,
    );

    engine.add_method(
        EXITCODE.type_id,
        "to_bool",
        MethodType::native(vec![], vec![], BOOL, gen.next()),
    );

    for op in ARITHMETIC_OPERATORS {
        engine.add_method(
            INT.type_id,
            name_operator_method(*op),
            MethodType::native(vec![], vec![INT], INT, gen.next()),
        );
        engine.add_method(
            FLOAT.type_id,
            name_operator_method(*op),
            MethodType::native(vec![], vec![FLOAT], FLOAT, gen.next()),
        );
    }
    engine.add_method(
        INT.type_id,
        name_operator_method(BinaryOperator::Modulo),
        MethodType::native(vec![], vec![INT], INT, gen.next()),
    );
    engine.add_method(
        BOOL.type_id,
        "not",
        MethodType::native(vec![], vec![], BOOL, gen.next()),
    );
    for ty in [BOOL, STRING] {
        for op in EQUALITY_OPERATORS {
            engine.add_method(
                ty.type_id,
                name_operator_method(*op),
                MethodType::native(vec![], vec![ty], BOOL, gen.next()),
            );
        }
    }
    for ty in [INT, FLOAT] {
        for op in COMPARISON_OPERATORS {
            engine.add_method(
                ty.type_id,
                name_operator_method(*op),
                MethodType::native(vec![], vec![ty], BOOL, gen.next()),
            );
        }
    }
    for stringify in [BOOL, EXITCODE, INT, FLOAT] {
        engine.add_method(
            stringify.type_id,
            "to_string",
            MethodType::native(vec![], vec![], STRING, gen.next()),
        );
    }
    engine.add_method(
        INT.type_id,
        "to_float",
        MethodType::native(vec![], vec![], FLOAT, gen.next()),
    );

    engine.add_method(
        STRING.type_id,
        "len",
        MethodType::native(vec![], vec![], INT, gen.next()),
    );
    engine.add_method(
        STRING.type_id,
        name_operator_method(BinaryOperator::Plus),
        MethodType::native(vec![], vec![STRING], STRING, gen.next()),
    );

    engine.add_generic(GENERIC_VECTOR.type_id, generic_param1);

    engine.add_method(
        GENERIC_VECTOR.type_id,
        "[]",
        MethodType::native(vec![], vec![INT], generic_param1, gen.next()),
    );

    engine.add_method(
        GENERIC_VECTOR.type_id,
        "push",
        MethodType::native(vec![], vec![generic_param1], UNIT, gen.next()),
    );

    engine.add_method(
        GENERIC_VECTOR.type_id,
        "pop",
        MethodType::native(
            vec![],
            vec![],
            TypeRef::new(LANG_REEF, opt_type),
            gen.next(),
        ),
    );
    engine.add_method(
        GENERIC_VECTOR.type_id,
        "len",
        MethodType::native(vec![], vec![], INT, gen.next()),
    );

    engine.add_method(
        STRING.type_id,
        "split",
        MethodType::native(
            vec![],
            vec![STRING],
            TypeRef::new(LANG_REEF, TypeId(10)), //Vec[String] instance
            gen.next(),
        ),
    );
    engine.add_method(
        STRING.type_id,
        "bytes",
        MethodType::native(
            vec![],
            vec![],
            TypeRef::new(LANG_REEF, TypeId(11)), // Vec[Int] instance
            gen.next(),
        ),
    );

    for operand in [BOOL, EXITCODE] {
        for op in LOGICAL_OPERATORS {
            engine.add_method(
                operand.type_id,
                name_operator_method(*op),
                MethodType::native(vec![], vec![operand], operand, gen.next()),
            );
        }
    }
    for operand in [INT, FLOAT] {
        engine.add_method(
            operand.type_id,
            "neg",
            MethodType::native(vec![], vec![], operand, gen.next()),
        );
    }

    engine.add_generic(GENERIC_OPTION.type_id, generic_param1);
    engine.add_method(
        GENERIC_OPTION.type_id,
        "is_none",
        MethodType::native(vec![], vec![], BOOL, gen.next()),
    );
    engine.add_method(
        GENERIC_OPTION.type_id,
        "is_some",
        MethodType::native(vec![], vec![], BOOL, gen.next()),
    );
    engine.add_method(
        GENERIC_OPTION.type_id,
        "unwrap",
        MethodType::native(vec![], vec![], generic_param1, gen.next()),
    );
    engine.add_method(
        GENERIC_VECTOR.type_id,
        "[]",
        MethodType::native(vec![], vec![INT, generic_param1], UNIT, gen.next()),
    );

    engine.add_method(
        INT.type_id,
        "to_exitcode",
        MethodType::native(vec![], vec![], EXITCODE, gen.next()),
    );
    engine.add_method(
        EXITCODE.type_id,
        "to_int",
        MethodType::native(vec![], vec![], INT, gen.next()),
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

    ctx.push_local_typed(SourceId(0), ERROR);
    ctx.push_local_typed(SourceId(0), NOTHING);
    ctx.push_local_typed(SourceId(0), UNIT);
    ctx.push_local_typed(SourceId(0), BOOL);
    ctx.push_local_typed(SourceId(0), EXITCODE);
    ctx.push_local_typed(SourceId(0), INT);
    ctx.push_local_typed(SourceId(0), FLOAT);
    ctx.push_local_typed(SourceId(0), STRING);
    ctx.push_local_typed(SourceId(0), GENERIC_VECTOR);
    ctx.push_local_typed(SourceId(0), GENERIC_OPTION);
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
