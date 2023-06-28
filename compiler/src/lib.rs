use std::io;
use std::io::Write;

use analyzer::types::hir::TypedExpr;

use crate::bytecode::Bytecode;
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState};

pub mod bytecode;
mod constant_pool;
mod emit;
mod r#type;

pub fn compile(expr: &TypedExpr, writer: &mut impl Write) -> Result<(), io::Error> {
    let mut emitter = Bytecode::default();
    let mut cp = ConstantPool::default();
    emit(expr, &mut emitter, &mut cp, &mut EmissionState::new());

    write(writer, emitter, cp)
}

fn write(writer: &mut impl Write, emitter: Bytecode, pool: ConstantPool) -> Result<(), io::Error> {
    writer.write_all(
        &u32::try_from(pool.strings.len())
            .expect("too many strings in constant pool")
            .to_be_bytes(),
    )?;
    for constant in pool.strings {
        writer.write_all(&constant.len().to_be_bytes())?;
        writer.write_all(constant.as_bytes())?;
    }
    writer.write_all(&emitter.bytes)?;
    Ok(())
}
