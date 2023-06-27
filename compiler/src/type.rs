use analyzer::types::hir::TypeId;
use analyzer::types::*;

/// returns the size of a given type identifier
pub fn get_type_size(tpe: TypeId) -> TypeSize {
    match tpe {
        NOTHING => TypeSize::Zero,
        BOOL | EXIT_CODE => TypeSize::Byte,
        INT | FLOAT => TypeSize::QWord,
        ERROR => panic!("Received 'ERROR' type in compilation phase."),
        _ => TypeSize::Reference, //other types are object types which are references
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum TypeSize {
    Zero,
    Byte,
    QWord,
    Reference,
}

impl From<TypeId> for TypeSize {
    fn from(value: TypeId) -> Self {
        get_type_size(value)
    }
}
