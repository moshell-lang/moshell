use analyzer::types::ty::Type;

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
