use std::io;
use std::io::Write;

use analyzer::engine::Engine;
use analyzer::name::Name;
use analyzer::relations::LocalId;
use analyzer::types::engine::{Chunk, TypedEngine};
use analyzer::types::hir::ExprKind;
use analyzer::types::Typing;

use crate::bytecode::{Bytecode, Instructions};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState};
use crate::locals::LocalsLayout;
use crate::r#type::get_type_stack_size;

pub mod bytecode;
mod constant_pool;
mod emit;
mod locals;
mod r#type;

pub fn compile(
    typed_engine: &TypedEngine,
    engine: &Engine,
    typing: &Typing,
    writer: &mut impl Write,
) -> Result<(), io::Error> {
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

        compile_instruction_set(chunk, &mut bytecode, typing, engine, &mut cp);

        function_count += 1;
    }

    bytecode.patch_u32_placeholder(function_count_ph, function_count);

    write(writer, &bytecode, &cp)
}

fn compile_instruction_set(
    chunk: &Chunk,
    bytecode: &mut Bytecode,
    typing: &Typing,
    engine: &Engine,
    cp: &mut ConstantPool,
) {
    // emit the function's parameters bytes length
    let parameters_bytes_count: u32 = chunk
        .parameters
        .iter()
        .map(|p| Into::<u8>::into(get_type_stack_size(p.ty)) as u32)
        .sum();

    bytecode.emit_u32(parameters_bytes_count);
    // emit the function's return bytes count
    let return_bytes_count: u8 = get_type_stack_size(chunk.return_type).into();
    bytecode.emit_byte(return_bytes_count);

    // if the function returns void, we won't the compiler to emit useless values
    let use_value = return_bytes_count != 0;

    // emit instruction count placeholder
    let instruction_count = bytecode.emit_u32_placeholder();

    // emit locals byte count
    let locals_byte_count = bytecode.emit_u32_placeholder();

    let start = bytecode.len();

    let mut instructions = Instructions::wrap(bytecode);
    let mut locals = LocalsLayout::default();

    for (id, param) in chunk.parameters.iter().enumerate() {
        locals.expand_layout(LocalId(id), param.ty)
    }

    let mut state = EmissionState::default();

    macro_rules! emit_expr {
        ($expr:expr) => {
            emit(
                $expr,
                &mut instructions,
                typing,
                engine,
                cp,
                &mut locals,
                &mut state,
            )
        };
    }

    state.use_values(use_value);
    state.is_returning_value = use_value;

    match &chunk.expression.kind {
        ExprKind::Block(exprs) => {
            if let Some((last, head)) = exprs.split_last() {
                state.is_returning_value = false;
                for expr in head {
                    emit_expr!(expr);
                }
                state.is_returning_value = use_value;
                emit_expr!(last);
            }
        }
        _ => emit_expr!(&chunk.expression),
    }

    // patch instruction count placeholder
    let instruction_byte_count =
        u32::try_from(bytecode.len() - start).expect("too much instructions");
    bytecode.patch_u32_placeholder(instruction_count, instruction_byte_count);
    bytecode.patch_u32_placeholder(
        locals_byte_count,
        locals.length().max(return_bytes_count as u32),
    )
}

fn write(
    writer: &mut impl Write,
    bytecode: &Bytecode,
    pool: &ConstantPool,
) -> Result<(), io::Error> {
    #[cfg(target_pointer_width = "32")]
    writer.write_all(&[1])?;

    #[cfg(target_pointer_width = "64")]
    writer.write_all(&[2])?;

    #[cfg(all(not(target_pointer_width = "32"), not(target_pointer_width = "64")))]
    compile_error!(
        "Targeted architecture not supported, can only compile for 64 or 32 bits platforms"
    );

    write_constant_pool(pool, writer)?;
    writer.write_all(&bytecode.bytes)?;
    Ok(())
}

fn write_constant_pool(cp: &ConstantPool, writer: &mut impl Write) -> Result<(), io::Error> {
    let pool_len = u32::try_from(cp.strings.len()).expect("constant pool too large");
    writer.write_all(&pool_len.to_be_bytes())?;

    for str in &cp.strings {
        let str_len = u32::try_from(str.len()).expect("string too large");
        writer.write_all(&str_len.to_be_bytes())?;
        writer.write_all(str.as_bytes())?;
    }
    Ok(())
}
