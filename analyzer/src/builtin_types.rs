use crate::types::Type;

pub fn bool_type() -> Type {
    Type::Constructed("Bool".to_owned(), vec![])
}
pub fn int_type() -> Type {
    Type::Constructed("Int".to_owned(), vec![])
}
pub fn float_type() -> Type {
    Type::Constructed("Float".to_owned(), vec![])
}
pub fn string_type() -> Type {
    Type::Constructed("Str".to_owned(), vec![])
}
pub fn nil_type() -> Type {
    Type::Constructed("Nil".to_owned(), vec![])
}
pub fn nothing_type() -> Type {
    Type::Constructed("Nothing".to_owned(), vec![])
}
