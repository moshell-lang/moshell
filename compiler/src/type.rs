use analyzer::types::hir::TypeId;
use analyzer::types::ty::Type;
use analyzer::types::*;

use crate::constant_pool::ConstantPool;

pub fn transform_to_vm_type(tpe: &Type, cp: &mut ConstantPool) -> u32 {
    let type_identifier = match tpe {
        Type::Bool | Type::ExitCode => "byte",
        Type::Int => "int",
        Type::Float => "float",
        Type::Unit | Type::Nothing => "void", //zero sized types
        Type::String => "String",
        Type::Error | Type::Unknown => {
            panic!("{tpe} is not a compilable type")
        }
        Type::Function(_) => {
            panic!("Can only support primitives")
        }
    };
    cp.insert_string(type_identifier)
}

/// returns the size in bytes of a given type
pub fn get_type_size(tpe: &TypeId) -> TypeSize {
    match *tpe {
        NOTHING | UNIT => TypeSize::Zero,
        BOOL | EXIT_CODE => TypeSize::Byte,
        INT | FLOAT => TypeSize::QWord,
        ERROR  => panic!("Received 'ERROR' type in compilation phase."),
        _ => TypeSize::QWord, //other types are object types whose reference size is 8
    }
}

pub enum TypeSize {
    Zero = 0,
    Byte = 1,
    QWord = 8
}

impl Into<TypeSize> for &TypeId {
    fn into(self) -> TypeSize {
        get_type_size(self)
    }
}