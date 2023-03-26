use crate::types::{DefinedType};

pub fn unit() -> DefinedType {
    DefinedType::cons("Unit")
}

pub fn int() -> DefinedType {
    DefinedType::cons("Int")
}

pub fn float() -> DefinedType {
    DefinedType::cons("Float")
}

pub fn exitcode() -> DefinedType {
    DefinedType::cons("Exitcode")
}

pub fn str() -> DefinedType {
    DefinedType::cons("Str")
}

pub fn bool() -> DefinedType {
    DefinedType::cons("Bool")
}





