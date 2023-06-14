use std::io;
use std::io::Write;

use analyzer::types::hir::TypedExpr;

use crate::bytecode::Bytecode;
use crate::emit::emit;

pub mod bytecode;
mod emit;


pub fn compile(expr: &TypedExpr, writer: &mut impl Write) -> Result<(), io::Error> {
    let mut emitter = Bytecode::default();
    emit(&mut emitter, expr);
    write(writer, emitter)
}

fn write(writer: &mut impl Write, emitter: Bytecode) -> Result<(), io::Error> {
    writer.write_all(&[emitter.constants.len() as u8])?;
    for constant in emitter.constants {
        writer.write_all(&constant.len().to_be_bytes())?;
        writer.write_all(constant.as_bytes())?;
    }
    writer.write_all(&emitter.bytes)?;
    Ok(())
}
