use analyzer::types::hir::TypeId;
use analyzer::types::ty::Type;
use analyzer::types::*;
use std::mem::size_of;

/// Transforms given type name to a type name compatible with bytecode specifications.
pub fn type_to_bytecode_str(tpe: &Type) -> &'static str {
    match tpe {
        Type::Bool | Type::ExitCode => "B",
        Type::Int => "I",
        Type::Float => "F",
        Type::String => "S",
        Type::Unit | Type::Nothing => "V", //zero sized types
        Type::Error | Type::Unknown => {
            panic!("{tpe} is not a compilable type")
        }
        // object types are not yet supported
        Type::Function(_) => {
            panic!("Can only support primitives")
        }
    }
}

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

impl From<ValueStackSize> for u8 {
    fn from(val: ValueStackSize) -> Self {
        match val {
            ValueStackSize::Zero => 0,
            ValueStackSize::Byte => 1,
            ValueStackSize::QWord => 8,
            // As the size of an address is platform dependant, the bytecode specification
            // defines a flag to set the type of architecture the bytecode is compiled for
            ValueStackSize::Reference => size_of::<*const u8>() as u8,
        }
    }
}

impl From<TypeId> for ValueStackSize {
    fn from(value: TypeId) -> Self {
        get_type_stack_size(value)
    }
}
