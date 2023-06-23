use std::io;
use std::io::Write;
use std::iter::once;

use analyzer::engine::Engine;
use analyzer::types::{NOTHING, Typing, UNIT};
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

    let mut scripts = Vec::new();

    let function_count_ph = bytecode.emit_u32_placeholder();
    let mut function_count = 0;
    for (id, chunk) in typed_engine.iter_chunks() {
        if chunk.return_type == UNIT {
            scripts.push(chunk);
            continue;
        }
        let chunk_fqn = &engine.get_environment(id).unwrap().fqn;
        let params: Vec<_> = chunk.parameters
            .iter()
            .map(|p| p.ty)
            .collect();
        let signature = FunctionSignature::make(
            chunk_fqn.simple_name(),
            &params,
            chunk.return_type,
            typing,
            &mut cp,
        );
        bytecode.emit_constant_ref(cp.insert_signature(signature));
        compile_instruction_set(
            get_type_size(chunk.return_type) != TypeSize::Zero,
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
    let signature = FunctionSignature::make("<module_main>", &[], NOTHING, typing, &mut cp);
    bytecode.emit_constant_ref(cp.insert_signature(signature));
    compile_instruction_set(
        false,
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
    use_value: bool,
    expressions: impl Iterator<Item = &'a TypedExpr>,
    bytecode: &mut Bytecode,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
) {
    // emit instruction count placeholder
    let instruction_count = bytecode.emit_u32_placeholder();
    let start = bytecode.len();
    // emit operand capacity placeholder
    let operand_capacity = bytecode.emit_u32_placeholder();

    // write instructions
    let mut instructions = Instructions::wrap(bytecode);

    let mut state = EmissionState::default();
    state.use_values(use_value);
    for expr in expressions {
        emit(
            expr,
            &mut instructions,
            typing,
            engine,
            cp,
            &mut state,
        );
    }

    // test if the
    match instructions.push_offset {
        0 if use_value => panic!("Compilation lets an empty operand stack but value is needed"),
        2.. if use_value => panic!("Compilation lets an operand stack with more than one value"),
        1.. if !use_value => panic!("Compilation lets a non empty operand stack which will never be used"),
        _ => () //no issue with operand stack
    }

    if use_value && instructions.push_offset == 0 {

    }

    let operands_capacity = instructions.max_operand_stack_size;

    // patch operand capacity placeholder
    bytecode.patch_u32_placeholder(operand_capacity, operands_capacity);
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
