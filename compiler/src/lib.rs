use std::io;
use std::io::Write;

use analyzer::engine::Engine;
use analyzer::environment::variables::Variables;
use analyzer::name::Name;
use analyzer::relations::{LocalId, Relations, SourceId};
use analyzer::types::engine::{Chunk, TypedEngine};

use crate::bytecode::{Bytecode, Instructions};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState};
use crate::locals::LocalsLayout;
use crate::r#type::{get_type_stack_size, ValueStackSize};

pub mod bytecode;
mod constant_pool;
mod emit;
mod locals;
mod r#type;

/// contains all the data related with analysis process
pub struct Analysis<'a> {
    engine: &'a Engine<'a>,
    relations: &'a Relations,
}

pub fn compile(
    typed_engine: &TypedEngine,
    engine: &Engine,
    relations: &Relations,
    writer: &mut impl Write,
) -> Result<(), io::Error> {
    let analysis = Analysis {
        engine,
        relations,
    };

    let mut bytecode = Bytecode::default();
    let mut cp = ConstantPool::default();

    //have we already met a script's main method ?
    // compiler cannot currently handle multiple modules so this flag is meant to make the compiler panic
    // if more than one script chunk is detected
    let mut is_main_compiled = false;

    let function_count_ph = bytecode.emit_u32_placeholder();
    let mut function_count = 0;
    for (id, chunk) in typed_engine.iter_chunks() {
        let chunk_env = engine.get_environment(id).unwrap();
        let chunk_fqn = &chunk_env.fqn;

        let name = if chunk.is_script {
            if is_main_compiled {
                todo!("Compiler cannot support multiple modules")
            }
            is_main_compiled = true;
            chunk_fqn.appended(Name::new("<main>"))
        } else {
            chunk_fqn.clone()
        };

        // emit the function's name
        let signature_idx = cp.insert_string(name);
        bytecode.emit_constant_ref(signature_idx);

        compile_chunk(
            chunk,
            &chunk_env.variables,
            id,
            &mut bytecode,
            &analysis,
            &mut cp,
        );

        function_count += 1;
    }

    bytecode.patch_u32_placeholder(function_count_ph, function_count);

    write(writer, &bytecode, &cp)
}

fn compile_chunk(
    chunk: &Chunk,
    chunk_vars: &Variables,
    chunk_id: SourceId,
    bytecode: &mut Bytecode,
    analysis: &Analysis,
    cp: &mut ConstantPool,
) {
    let locals_byte_count = bytecode.emit_u32_placeholder();

    let captures_count = chunk_vars
        .external_vars()
        .count();
    // compute the chunk's parameters bytes length
    let parameters_bytes_count: u32 = {
        let explicit_params_count: u32 = chunk
            .parameters
            .iter()
            .map(|p| Into::<u8>::into(get_type_stack_size(p.ty)) as u32)
            .sum::<u32>();
        let captures_params_count: u32 = captures_count as u32 * u8::from(ValueStackSize::Reference) as u32;
        explicit_params_count + captures_params_count
    };

    bytecode.emit_u32(parameters_bytes_count);
    // emit the function's return bytes count
    let return_bytes_count: u8 = get_type_stack_size(chunk.return_type).into();
    bytecode.emit_byte(return_bytes_count);

    let use_value = return_bytes_count != 0;

    // emit instruction count placeholder
    let instruction_count = bytecode.emit_u32_placeholder();

    let mut instructions = Instructions::wrap(bytecode);
    let mut locals = LocalsLayout::new(chunk_vars.all_vars().len() + captures_count);

    // set space for explicit parameters
    for (id, param) in chunk.parameters.iter().enumerate() {
        locals.set_value_space(LocalId(id), param.ty.into())
    }

    // set space for implicit captures
    for (_, id) in chunk_vars.external_vars() {
        locals.set_external_ref_space(id)
    }

    let mut state = EmissionState::new(use_value, chunk_id);

    emit(
        &chunk.expression,
        &mut instructions,
        analysis,
        cp,
        &mut locals,
        &mut state,
    );

    // patch instruction count placeholder
    let instruction_byte_count = instructions.current_ip();
    bytecode.patch_u32_placeholder(instruction_count, instruction_byte_count);

    let locals_length = locals.byte_count();
    bytecode.patch_u32_placeholder(locals_byte_count, locals_length)
}

fn write(
    writer: &mut impl Write,
    bytecode: &Bytecode,
    pool: &ConstantPool,
) -> Result<(), io::Error> {
    write_constant_pool(pool, writer)?;
    writer.write_all(bytecode.bytes())
}

fn write_constant_pool(cp: &ConstantPool, writer: &mut impl Write) -> Result<(), io::Error> {
    let pool_len = u32::try_from(cp.strings.len()).expect("constant pool too large");
    writer.write_all(&pool_len.to_be_bytes())?;

    for str in &cp.strings {
        writer.write_all(&(str.len() as u64).to_be_bytes())?;
        writer.write_all(str.as_bytes())?;
    }
    Ok(())
}
