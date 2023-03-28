use crate::types::{DefinedType, ParametrizedType};

//LANG TYPES
pub fn unit() -> DefinedType {
    DefinedType::Parametrized(ParametrizedType::cons("Unit"))
}

pub fn int() -> DefinedType {
    DefinedType::Parametrized(ParametrizedType::cons("Int"))
}

pub fn float() -> DefinedType {
    DefinedType::Parametrized(ParametrizedType::cons("Float"))
}

pub fn exitcode() -> DefinedType {
    DefinedType::Parametrized(ParametrizedType::cons("Exitcode"))
}

pub fn str() -> DefinedType {
    DefinedType::Parametrized(ParametrizedType::cons("Str"))
}

pub fn bool() -> DefinedType {
    DefinedType::Parametrized(ParametrizedType::cons("Bool"))
}

//STD TYPES
//TODO: Option, List, Iterable, Try, Ok, Err, None, Some



