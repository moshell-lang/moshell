use analyzer::types::hir::TypeId;
use analyzer::types::{BOOL, ERROR, EXIT_CODE, FLOAT, INT, NOTHING, UNIT};

/// returns the size of a given type identifier
pub fn get_type_stack_size(tpe: TypeId) -> ValueStackSize {
    match tpe {
        NOTHING | UNIT => ValueStackSize::Zero,
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

impl From<ValueStackSize> for u8 {
    fn from(val: ValueStackSize) -> Self {
        match val {
            ValueStackSize::Zero => 0,
            ValueStackSize::Byte => 1,
            ValueStackSize::QWord | ValueStackSize::Reference => 8,
        }
    }
}

impl From<TypeId> for ValueStackSize {
    fn from(value: TypeId) -> Self {
        get_type_stack_size(value)
    }
}
