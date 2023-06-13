use crate::relations::NativeObjectId;
use crate::types::engine::TypedEngine;
use crate::types::operator::name_operator_method;
use crate::types::ty::MethodType;
use crate::types::{FLOAT, INT, STRING};
use ast::operation::BinaryOperator;

const ARITHMETIC_OPERATORS: &[BinaryOperator] = &[
    BinaryOperator::Plus,
    BinaryOperator::Minus,
    BinaryOperator::Times,
    BinaryOperator::Divide,
    BinaryOperator::Modulo,
];

pub fn lang(engine: &mut TypedEngine) {
    let mut id = 0usize;
    for op in ARITHMETIC_OPERATORS {
        engine.add_method(
            INT,
            name_operator_method(*op),
            MethodType::native(vec![INT], INT, NativeObjectId(id)),
        );
        engine.add_method(
            FLOAT,
            name_operator_method(*op),
            MethodType::native(vec![FLOAT], FLOAT, NativeObjectId(id + 1)),
        );
        id += 2;
    }
    for stringify in [INT, FLOAT, STRING] {
        engine.add_method(
            stringify,
            "to_string",
            MethodType::native(vec![], STRING, NativeObjectId(id)),
        );
        id += 1;
    }
    engine.add_method(
        STRING,
        "len",
        MethodType::native(vec![], INT, NativeObjectId(id)),
    );
    engine.add_method(
        STRING,
        name_operator_method(BinaryOperator::Plus),
        MethodType::native(vec![STRING], STRING, NativeObjectId(id + 1)),
    );
}
