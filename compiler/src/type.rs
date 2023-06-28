use analyzer::types::hir::TypeId;
use analyzer::types::*;

/// returns the size of a given type identifier
pub fn get_type_stack_size(tpe: TypeId) -> ValueStackSize {
    match tpe {
        NOTHING => ValueStackSize::Zero,
        BOOL | EXIT_CODE => ValueStackSize::Byte,
        INT | FLOAT => ValueStackSize::QWord,
        ERROR => panic!("Received 'ERROR' type in compilation phase."),
        _ => ValueStackSize::Reference, //other types are object types which are references
    }
}

/// Different sizes a value can have on the stack.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum ValueStackSize {
    Zero,
    Byte,
    QWord,
    Reference,
}

impl From<TypeId> for ValueStackSize {
    fn from(value: TypeId) -> Self {
        get_type_stack_size(value)
    }
}
