use crate::relations::NativeId;
use crate::types::engine::TypedEngine;
use crate::types::operator::name_operator_method;
use crate::types::ty::MethodType;
use crate::types::{BOOL, EXIT_CODE, FLOAT, INT, STRING};
use ast::operation::BinaryOperator;

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
pub fn lang(engine: &mut TypedEngine) {
    let mut gen = VariableGenerator::default();
    engine.add_method(
        EXIT_CODE,
        "to_bool",
        MethodType::native(vec![], BOOL, gen.next()),
    );
    for op in ARITHMETIC_OPERATORS {
        engine.add_method(
            INT,
            name_operator_method(*op),
            MethodType::native(vec![INT], INT, gen.next()),
        );
        engine.add_method(
            FLOAT,
            name_operator_method(*op),
            MethodType::native(vec![FLOAT], FLOAT, gen.next()),
        );
    }
    engine.add_method(
        INT,
        name_operator_method(BinaryOperator::Modulo),
        MethodType::native(vec![INT], INT, gen.next()),
    );
    for op in [BinaryOperator::EqualEqual, BinaryOperator::NotEqual] {
        engine.add_method(
            BOOL,
            name_operator_method(op),
            MethodType::native(vec![BOOL], BOOL, gen.next()),
        );
    }
    for ty in [INT, FLOAT] {
        for op in COMPARISON_OPERATORS {
            engine.add_method(
                ty,
                name_operator_method(*op),
                MethodType::native(vec![ty], BOOL, gen.next()),
            );
        }
    }
    for stringify in [BOOL, EXIT_CODE, INT, FLOAT] {
        engine.add_method(
            stringify,
            "to_string",
            MethodType::native(vec![], STRING, gen.next()),
        );
    }
    engine.add_method(
        INT,
        "to_float",
        MethodType::native(vec![], FLOAT, gen.next()),
    );
    engine.add_method(STRING, "len", MethodType::native(vec![], INT, gen.next()));
    engine.add_method(
        STRING,
        name_operator_method(BinaryOperator::Plus),
        MethodType::native(vec![STRING], STRING, gen.next()),
    );
}
