use analyzer::typing::user::{self, TypeId};

/// returns the size of a given type identifier
pub fn get_type_stack_size(tpe: TypeId) -> ValueStackSize {
    match tpe {
        user::NOTHING_TYPE | user::UNIT_TYPE => ValueStackSize::Zero,
        user::BOOL_TYPE | user::EXITCODE_TYPE => ValueStackSize::Byte,
        user::UNKNOWN_TYPE | user::ERROR_TYPE => {
            panic!("Received '`{tpe:?}`' type in compilation phase.")
        }
        _ => ValueStackSize::QWord, // other types are object types which are references (q-words)
    }
}

/// Different sizes a value can have on the stack.
#[derive(Copy, Clone, Eq, Debug, PartialEq)]
pub enum ValueStackSize {
    Zero = 0,
    Byte = 1,
    QWord = 8,
}

impl From<ValueStackSize> for u8 {
    fn from(val: ValueStackSize) -> Self {
        val as u8
    }
}

impl From<TypeId> for ValueStackSize {
    fn from(value: TypeId) -> Self {
        get_type_stack_size(value)
    }
}
