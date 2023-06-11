use crate::types::engine::TypedEngine;
use crate::types::ty::MethodType;
use crate::types::{FLOAT, INT, STRING};

pub fn lang(engine: &mut TypedEngine) {
    int_type(engine);
    float_type(engine);
    string_type(engine);
}

fn int_type(engine: &mut TypedEngine) {
    for op in ["add", "sub", "mul", "div", "mod"] {
        engine.add_method(INT, op, MethodType::native(vec![INT], INT));
    }
}

fn float_type(engine: &mut TypedEngine) {
    for op in ["add", "sub", "mul", "div", "mod"] {
        engine.add_method(FLOAT, op, MethodType::native(vec![FLOAT], FLOAT));
    }
}

fn string_type(engine: &mut TypedEngine) {
    engine.add_method(STRING, "len", MethodType::native(vec![], INT));
}
