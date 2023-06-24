use std::io;
use std::io::Write;

use analyzer::engine::Engine;
use analyzer::types::{Typing, UNIT};
use analyzer::types::engine::TypedEngine;
use analyzer::types::hir::TypedExpr;

use crate::bytecode::{Bytecode, Instructions};
use crate::constant_pool::{ConstantPool, FunctionSignature, PoolConstant};
use crate::emit::{EmissionState, emit};
use crate::r#type::{get_type_size, TypeSize};

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

    //have we already met a script main method ?
    // compiler cannot currently handle multiple modules so this flag is meant to make the compiler panic
    // if two script chunks are detected
    let mut is_main_compiled = false;

    let function_count_ph = bytecode.emit_u32_placeholder();
    let mut function_count = 0;
    for (id, chunk) in typed_engine.iter_chunks() {
        let chunk_env = engine.get_environment(id).unwrap();
        let chunk_fqn = &chunk_env.fqn;
        let params: Vec<_> = chunk.parameters
            .iter()
            .map(|p| p.ty)
            .collect();

        let signature = if chunk.is_script {
            if is_main_compiled {
                todo!("Compiler cannot support multiple modules")
            }
            is_main_compiled = true;
            FunctionSignature::make("<main>", &[], UNIT, typing, &mut cp)
        } else {
            FunctionSignature::make(
                &chunk_fqn.to_string(),
                &params,
                chunk.return_type,
                typing,
                &mut cp,
            )
        };

        let locals_count = chunk_env.variables.all_vars().len() as u32;

        let signature_idx = cp.insert_signature(signature);
        bytecode.emit_constant_ref(signature_idx);
        compile_instruction_set(
            locals_count,
            get_type_size(chunk.return_type) != TypeSize::Zero,
            &chunk.expression,
            &mut bytecode,
            typing,
            engine,
            &mut cp,
        );

        function_count += 1;
    }

    bytecode.patch_u32_placeholder(function_count_ph, function_count);

    write(writer, &bytecode, &cp)
}

fn compile_instruction_set(
    locals_count: u32,
    use_value: bool,
    expression: &TypedExpr,
    bytecode: &mut Bytecode,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
) {
    // emit instruction count placeholder
    let instruction_count = bytecode.emit_u32_placeholder();

    // emit locals count
    bytecode.emit_u32(locals_count);

    let start = bytecode.len();

    // write instructions
    let mut instructions = Instructions::wrap(bytecode);

    let mut state = EmissionState::default();
    state.use_values(use_value);
    emit(
        expression,
        &mut instructions,
        typing,
        engine,
        cp,
        &mut state,
    );
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
