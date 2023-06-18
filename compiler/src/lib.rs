use std::io;
use std::io::Write;

use analyzer::engine::Engine;
use analyzer::name::Name;
use analyzer::types::{NOTHING, Typing};
use analyzer::types::engine::{Chunk, TypedEngine};

use crate::bytecode::{Bytecode, FileInstructions, FileOpcode, Instructions};
use crate::constant_pool::{ConstantPool, FunctionSignature, PoolConstant};
use crate::emit::{EmissionState, emit};

pub mod bytecode;
mod constant_pool;
mod emit;


pub fn compile(typed_engine: &TypedEngine, engine: &Engine, typing: &Typing, writer: &mut impl Write) -> Result<(), io::Error> {
    let mut bytecode = Bytecode::default();
    let mut cp = ConstantPool::default();

    let mut scripts = Vec::new();
    for (id, chunk) in typed_engine.iter_chunks() {
        if chunk.return_type == NOTHING {
            scripts.push(chunk);
            continue;
        }
        let chunk_fqn = &engine.get_environment(id).unwrap().fqn;
        compile_function_declaration(chunk, chunk_fqn, typing, &mut bytecode, &mut cp);
    }

    // write in scripts codes. scripts and functions are intentionally appended in same file
    // because file paths are not yet handled in VM
    bytecode.emit_byte(FileOpcode::ScriptCode as u8);
    let instructions = &mut Instructions::wrap(&mut bytecode);
    for script in scripts {
        emit(&script.expression, instructions, typing, &mut cp, &mut EmissionState::default());
    }

    write(writer, &bytecode, &cp)
}

fn compile_function_declaration(chunk: &Chunk, fqn: &Name, typing: &Typing, bytecode: &mut Bytecode, cp: &mut ConstantPool) {
    let mut emitter = FileInstructions::wrap(bytecode);

    // start function declaration
    emitter.emit_code(FileOpcode::StartFun);
    // name
    bytecode.emit_constant_ref(cp.insert_string(&fqn.to_string()));


    // write signature's reference in constant pool
    let params = chunk.parameters.iter().map(|p| p.ty);
    let signature = FunctionSignature::make(params, chunk.return_type, typing, cp);
    bytecode.emit_constant_ref(cp.insert_signature(signature));

    // emit body instruction count placeholder
    let body_size = bytecode.emit_u32_placeholder();
    let start = bytecode.len();

    // write body instructions
    let mut instructions = Instructions::wrap(bytecode);
    emit(&chunk.expression, &mut instructions, typing, cp, &mut EmissionState::default());

    // patch body instruction count body
    bytecode.patch_u32_placeholder(body_size, (bytecode.len() - start) as u32);
}


fn write(writer: &mut impl Write, bytecode: &Bytecode, pool: &ConstantPool) -> Result<(), io::Error> {
    write_constant_pool(pool, writer)?;
    writer.write_all(&bytecode.bytes)?;
    Ok(())
}

fn write_constant_pool(cp: &ConstantPool, writer: &mut impl Write) -> Result<(), io::Error> {
    writer.write_all(&[cp.constants.len() as u8])?;

    for constant in &cp.constants {
        match constant {
            PoolConstant::String(str) => {
                writer.write_all(&str.len().to_be_bytes())?;
                writer.write_all(str.as_bytes())?;
            }
            PoolConstant::Signature(sig) => {
                writer.write_all(&[sig.params.len() as u8])?;
                writer.write_all(&sig.params.iter().flat_map(|r| r.to_be_bytes()).collect::<Vec<_>>())?;
                writer.write_all(&sig.ret.to_be_bytes())?;
            }
        }
    }
    Ok(())
}

