use crate::types::types::{DefinedType, ParameterizedType};

//LANG TYPES

pub fn any() -> DefinedType {
    DefinedType::Parameterized(ParameterizedType::cons("Any"))
}

pub fn unit() -> DefinedType {
    DefinedType::Parameterized(ParameterizedType::cons("Unit"))
}

pub fn int() -> DefinedType {
    DefinedType::Parameterized(ParameterizedType::cons("Int"))
}

pub fn float() -> DefinedType {
    DefinedType::Parameterized(ParameterizedType::cons("Float"))
}

pub fn exitcode() -> DefinedType {
    DefinedType::Parameterized(ParameterizedType::cons("Exitcode"))
}

pub fn str() -> DefinedType {
    DefinedType::Parameterized(ParameterizedType::cons("Str"))
}

pub fn bool() -> DefinedType {
    DefinedType::Parameterized(ParameterizedType::cons("Bool"))
}



