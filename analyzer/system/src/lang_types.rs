use crate::types::ParameterizedType;

//LANG TYPES

pub fn any() -> ParameterizedType {
    ParameterizedType::cons("Any")
}

pub fn unit() -> ParameterizedType {
    ParameterizedType::cons("Unit")
}

pub fn int() -> ParameterizedType {
    ParameterizedType::cons("Int")
}

pub fn float() -> ParameterizedType {
    ParameterizedType::cons("Float")
}

pub fn exitcode() -> ParameterizedType {
    ParameterizedType::cons("Exitcode")
}

pub fn str() -> ParameterizedType {
    ParameterizedType::cons("Str")
}

pub fn bool() -> ParameterizedType {
    ParameterizedType::cons("Bool")
}
