use ast::operation::BinaryOperator;

use crate::engine::Engine;

use crate::reef::{Reef, LANG_REEF};
use crate::relations::{LocalId, Relations, SourceId};
use crate::types::ctx::TypeContext;
use crate::types::engine::{StructureId, TypedEngine};
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
pub const UNIT_STRUCT: StructureId = StructureId(0);
pub const BOOL_STRUCT: StructureId = StructureId(1);
pub const EXITCODE_STRUCT: StructureId = StructureId(2);
pub const INT_STRUCT: StructureId = StructureId(3);
pub const FLOAT_STRUCT: StructureId = StructureId(4);
pub const STRING_STRUCT: StructureId = StructureId(5);
pub const VEC_STRUCT: StructureId = StructureId(6);
pub const OPTION_STRUCT: StructureId = StructureId(7);
pub const GLOB_STRUCT: StructureId = StructureId(8);
pub const PID_STRUCT: StructureId = StructureId(9);

fn get_lang_struct_id(typing: &mut Typing, ty: TypeRef) -> StructureId {
    let Type::Structure(_, structure_id) = typing.get_type(ty.type_id).unwrap() else {
        panic!("given type is not a structured type")
    };
    *structure_id
}

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
        EXITCODE_STRUCT,
        "to_bool",
        MethodType::function(vec![], vec![], BOOL),
    );

    for op in ARITHMETIC_OPERATORS {
        engine.add_method(
            INT_STRUCT,
            name_operator_method(*op),
            MethodType::function(vec![], vec![INT], INT),
        );
        engine.add_method(
            FLOAT_STRUCT,
            name_operator_method(*op),
            MethodType::function(vec![], vec![FLOAT], FLOAT),
        );
    }
    engine.add_method(
        INT_STRUCT,
        name_operator_method(BinaryOperator::Modulo),
        MethodType::function(vec![], vec![INT], INT),
    );
    engine.add_method(
        BOOL_STRUCT,
        "not",
        MethodType::function(vec![], vec![], BOOL),
    );
    for ty in [BOOL, STRING] {
        let ty_structure = get_lang_struct_id(typing, ty);
        for op in EQUALITY_OPERATORS {
            engine.add_method(
                ty_structure,
                name_operator_method(*op),
                MethodType::function(vec![], vec![ty], BOOL),
            );
        }
    }
    for ty in [INT, FLOAT] {
        let ty_structure = get_lang_struct_id(typing, ty);
        for op in COMPARISON_OPERATORS {
            engine.add_method(
                ty_structure,
                name_operator_method(*op),
                MethodType::function(vec![], vec![ty], BOOL),
            );
        }
    }
    for struct_id in [BOOL_STRUCT, EXITCODE_STRUCT, INT_STRUCT, FLOAT_STRUCT] {
        engine.add_method(
            struct_id,
            "to_string",
            MethodType::function(vec![], vec![], STRING),
        );
    }
    engine.add_method(
        INT_STRUCT,
        "to_float",
        MethodType::function(vec![], vec![], FLOAT),
    );

    engine.add_method(
        STRING_STRUCT,
        "len",
        MethodType::function(vec![], vec![], INT),
    );
    engine.add_method(
        STRING_STRUCT,
        name_operator_method(BinaryOperator::Plus),
        MethodType::function(vec![], vec![STRING], STRING),
    );

    engine.add_generic(VEC_STRUCT, generic_param1.type_id);

    engine.add_method(
        VEC_STRUCT,
        "[]",
        MethodType::function(vec![], vec![INT], generic_param1),
    );

    engine.add_method(
        VEC_STRUCT,
        "push",
        MethodType::function(vec![], vec![generic_param1], UNIT),
    );

    engine.add_method(
        VEC_STRUCT,
        "pop",
        MethodType::function(vec![], vec![], TypeRef::new(LANG_REEF, opt_type)),
    );
    engine.add_method(VEC_STRUCT, "len", MethodType::function(vec![], vec![], INT));

    engine.add_method(
        STRING_STRUCT,
        "split",
        MethodType::function(vec![], vec![STRING], STRING_VEC),
    );
    engine.add_method(
        STRING_STRUCT,
        "bytes",
        MethodType::function(vec![], vec![], INT_VEC),
    );

    for operand in [BOOL, EXITCODE] {
        let operand_struct = get_lang_struct_id(typing, operand);
        for op in LOGICAL_OPERATORS {
            engine.add_method(
                operand_struct,
                name_operator_method(*op),
                MethodType::function(vec![], vec![operand], operand),
            );
        }
    }
    for operand in [INT, FLOAT] {
        let operand_struct = get_lang_struct_id(typing, operand);
        engine.add_method(
            operand_struct,
            "neg",
            MethodType::function(vec![], vec![], operand),
        );
    }

    engine.add_generic(OPTION_STRUCT, generic_param1.type_id);
    engine.add_method(
        OPTION_STRUCT,
        "is_none",
        MethodType::function(vec![], vec![], BOOL),
    );
    engine.add_method(
        OPTION_STRUCT,
        "is_some",
        MethodType::function(vec![], vec![], BOOL),
    );
    engine.add_method(
        OPTION_STRUCT,
        "unwrap",
        MethodType::function(vec![], vec![], generic_param1),
    );
    engine.add_method(
        VEC_STRUCT,
        "[]",
        MethodType::function(vec![], vec![INT, generic_param1], UNIT),
    );

    engine.add_method(
        INT_STRUCT,
        "to_exitcode",
        MethodType::function(vec![], vec![], EXITCODE),
    );
    engine.add_method(
        EXITCODE_STRUCT,
        "to_int",
        MethodType::function(vec![], vec![], INT),
    );

    engine.add_method(
        VEC_STRUCT,
        "pop_head",
        MethodType::function(vec![], vec![], generic_param1),
    );

    engine.add_method(
        GLOB_STRUCT,
        "spread",
        MethodType::function(vec![], vec![], STRING_VEC),
    );
    engine.add_method(
        PID_STRUCT,
        "to_string",
        MethodType::function(vec![], vec![], STRING),
    );
}

fn fill_lang_types(typing: &mut Typing, engine: &mut TypedEngine) {
    typing.add_type(Type::Error, Some("Error".to_string()));
    typing.add_type(Type::Nothing, Some("Nothing".to_string()));
    for primitive_name in [
        "Unit", "Bool", "Exitcode", "Int", "Float", "String", "Vec", "Option", "Glob", "Pid",
    ] {
        let structure_id = engine.init_empty_structure();
        typing.add_type(
            Type::Structure(None, structure_id),
            Some(primitive_name.to_string()),
        );
    }
    //init int vectors and string vectors (will be used by methods)
    typing.add_type(Type::Instantiated(GENERIC_VECTOR, vec![STRING]), None);
    typing.add_type(Type::Instantiated(GENERIC_VECTOR, vec![INT]), None);

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
        GLOB,
        PID,
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

    fill_lang_types(&mut reef.typing, &mut reef.typed_engine);
    fill_lang_bindings(&mut reef.type_context);
    fill_lang_typed_engine(&mut reef.typed_engine, &mut reef.typing);

    reef
}
