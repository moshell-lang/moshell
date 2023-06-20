use std::io;
use std::io::Write;
use std::iter::{empty, once};

use analyzer::engine::Engine;
use analyzer::types::engine::TypedEngine;
use analyzer::types::hir::TypedExpr;
use analyzer::types::{Typing, NOTHING};

use crate::bytecode::{Bytecode, Instructions};
use crate::constant_pool::{ConstantPool, FunctionSignature, PoolConstant};
use crate::emit::{emit, EmissionState};

pub mod bytecode;
mod constant_pool;
mod emit;
mod r#type;

pub fn compile(
    typed_engine: &TypedEngine,
    engine: &Engine,
    typing: &Typing,
    writer: &mut impl Write,
) -> Result<(), io::Error> {
    let mut bytecode = Bytecode::default();
    let mut cp = ConstantPool::default();

    let mut scripts = Vec::new();

    let function_count_ph = bytecode.emit_u32_placeholder();
    let mut function_count = 0;
    for (id, chunk) in typed_engine.iter_chunks() {
        if chunk.return_type == NOTHING {
            scripts.push(chunk);
            continue;
        }
        let chunk_fqn = &engine.get_environment(id).unwrap().fqn;
        let params = chunk.parameters.iter().map(|p| p.ty);
        let signature = FunctionSignature::make(
            chunk_fqn.simple_name(),
            params,
            chunk.return_type,
            typing,
            &mut cp,
        );
        bytecode.emit_constant_ref(cp.insert_signature(signature));
        compile_instruction_set(
            once(&chunk.expression),
            &mut bytecode,
            typing,
            engine,
            &mut cp,
        );

        function_count += 1;
    }

    // write in scripts codes. scripts and functions are intentionally appended in same file
    // because file paths are not yet handled in VM
    // the script codes are written in a <module_main> method
    // write main method signature
    let signature = FunctionSignature::make("<module_main>", empty(), NOTHING, typing, &mut cp);
    bytecode.emit_constant_ref(cp.insert_signature(signature));
    compile_instruction_set(
        scripts.iter().map(|c| &c.expression),
        &mut bytecode,
        typing,
        engine,
        &mut cp,
    );

    bytecode.patch_u32_placeholder(function_count_ph, function_count + 1); //add one for the main function

    write(writer, &bytecode, &cp)
}

fn compile_instruction_set<'a>(
    expressions: impl Iterator<Item = &'a TypedExpr>,
    bytecode: &mut Bytecode,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
) {
    // emit instruction count placeholder
    let instruction_count = bytecode.emit_u32_placeholder();
    let start = bytecode.len();

    // write instructions
    let mut instructions = Instructions::wrap(bytecode);
    for expr in expressions {
        emit(
            expr,
            &mut instructions,
            typing,
            engine,
            cp,
            &mut EmissionState::default(),
        );
    }

    // patch instruction count placeholder
    bytecode.patch_u32_placeholder(instruction_count, (bytecode.len() - start) as u32);
}

fn write(
    writer: &mut impl Write,
    bytecode: &Bytecode,
    pool: &ConstantPool,
) -> Result<(), io::Error> {
    write_constant_pool(pool, writer)?;
    writer.write_all(&bytecode.bytes)?;
    Ok(())
}

fn write_constant_pool(cp: &ConstantPool, writer: &mut impl Write) -> Result<(), io::Error> {
    writer.write_all(&[cp.constants.len() as u8])?;

    for constant in &cp.constants {
        match constant {
            PoolConstant::String(str) => {
                writer.write_all(&[1])?;
                writer.write_all(&str.len().to_be_bytes())?;
                writer.write_all(str.as_bytes())?;
            }
            PoolConstant::Signature(sig) => {
                writer.write_all(&[2])?;
                writer.write_all(&sig.name.to_be_bytes())?;
                writer.write_all(&[sig.params.len() as u8])?;
                writer.write_all(
                    &sig.params
                        .iter()
                        .flat_map(|r| r.to_be_bytes())
                        .collect::<Vec<_>>(),
                )?;
                writer.write_all(&sig.ret.to_be_bytes())?;
            }
        }
    }
    Ok(())
}
