use analyzer::types::ty::TypeRef;
use analyzer::types::{BOOL, ERROR, EXIT_CODE, FLOAT, INT, NOTHING, UNIT};

/// returns the size of a given type identifier
pub fn get_type_stack_size(tpe: TypeRef) -> ValueStackSize {
    match tpe {
        NOTHING | UNIT => ValueStackSize::Zero,
        BOOL | EXIT_CODE => ValueStackSize::Byte,
        INT | FLOAT => ValueStackSize::QWord,
        ERROR => panic!("Received 'ERROR' type in compilation phase."),
        _ => ValueStackSize::QWord, //other types are object types which are references (q-words)
    }
}

/// Different sizes a value can have on the stack.
#[derive(Copy, Clone, Eq, Debug, PartialEq)]
pub enum ValueStackSize {
    Zero,
    Byte,
    QWord,
}

impl From<ValueStackSize> for u8 {
    fn from(val: ValueStackSize) -> Self {
        match val {
            ValueStackSize::Zero => 0,
            ValueStackSize::Byte => 1,
            ValueStackSize::QWord => 8,
        }
    }
}

impl From<TypeRef> for ValueStackSize {
    fn from(value: TypeRef) -> Self {
        get_type_stack_size(value)
    }
}
