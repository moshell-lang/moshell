use analyzer::types::hir::TypeId;
use analyzer::types::ty::Type;
use analyzer::types::*;

use crate::constant_pool::ConstantPool;

/// Transforms given type name to a type name compatible with bytecode specification.
pub fn transform_to_primitive_type(tpe: &Type, cp: &mut ConstantPool) -> u32 {
    let type_identifier = match tpe {
        Type::Bool | Type::ExitCode => "byte",
        Type::Int => "int",
        Type::Float => "float",
        Type::String => "string",
        Type::Unit | Type::Nothing => "void", //zero sized types
        Type::Error | Type::Unknown => {
            panic!("{tpe} is not a compilable type")
        }
        // object types are not yet supported
        Type::Function(_) => {
            panic!("Can only support primitives")
        }
    };
    cp.insert_string(type_identifier)
}

/// returns the size of a given type identifier
pub fn get_type_size(tpe: TypeId) -> TypeSize {
    match tpe {
        NOTHING | UNIT => TypeSize::Zero,
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
    Reference
}

impl From<TypeId> for TypeSize {
    fn from(value: TypeId) -> Self {
        get_type_size(value)
    }
}
