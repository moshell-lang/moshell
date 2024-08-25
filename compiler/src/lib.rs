use std::ffi::OsStr;
use std::io;
use std::io::Write;

use ::context::source::ContentId;
use analyzer::hir::{Chunk, EncodableContent, ExprKind, NamedExports};
use analyzer::{Database, Reef};

use crate::bytecode::{Bytecode, InstructionPos, Instructions};
use crate::constant_pool::ConstantPool;
use crate::context::EmitterContext;
use crate::emit::{emit, EmissionState};
use crate::locals::LocalsLayout;
use crate::r#type::{get_type_stack_size, ValueStackSize};
use crate::structure::StructureLayout;

pub mod bytecode;
mod constant_pool;
mod context;
mod emit;
mod locals;
mod structure;
mod r#type;

pub trait SourceLineProvider {
    /// returns the line, starting from one, attributed to the given byte position of given content.
    fn get_line(&self, content: ContentId, byte_pos: usize) -> Option<usize>;
}

#[derive(Default)]
pub struct CompilerState {
    pub constant_pool: ConstantPool,
}

#[derive(Default)]
pub struct CompilerOptions<'a> {
    pub line_provider: Option<&'a dyn SourceLineProvider>,
    pub last_page_storage_var: Option<String>,
}

const MAPPINGS_ATTRIBUTE: u8 = 1;

pub fn compile_reef(
    database: &Database,
    reef: &Reef,
    writer: &mut impl Write,
    CompilerState { constant_pool: cp }: &mut CompilerState,
    options: CompilerOptions,
) -> Result<(), io::Error> {
    let mut bytecode = Bytecode::default();
    let layouts = Vec::<StructureLayout>::new();

    for EncodableContent {
        main,
        functions,
        exports,
    } in reef.group_by_content()
    {
        let ctx = EmitterContext {
            types: &database.checker.types,
            registry: &database.checker.registry,
            layouts: &layouts,
        };

        let mut page_size = 0u32;
        for (name, ty) in exports {
            let size = ValueStackSize::from(*ty) as u8;
            let offset = page_size;
            page_size += size as u32;
            cp.insert_exported(name, offset, ty.is_obj());
        }
        if options.last_page_storage_var.is_some() {
            page_size += u8::from(ValueStackSize::QWord) as u32;
        }

        compile_function_chunk(main, exports, &ctx, &mut bytecode, cp, &options);

        write_exported(cp, page_size, &mut bytecode)?;

        bytecode.emit_u32(layouts.len() as u32);
        bytecode.emit_u32(functions.len() as u32);

        for function in functions {
            compile_function_chunk(function, exports, &ctx, &mut bytecode, cp, &options);
        }
    }

    write(writer, &bytecode, cp)?;
    Ok(())
}

fn compile_function_chunk(
    chunk: &Chunk,
    exports: &NamedExports,
    ctx: &EmitterContext,
    bytecode: &mut Bytecode,
    cp: &mut ConstantPool,
    options: &CompilerOptions,
) {
    // emit the function's name
    let signature_idx = cp.insert_string(
        chunk
            .fqn
            .iter()
            .map(OsStr::to_string_lossy)
            .collect::<Vec<_>>()
            .join("::"),
    );
    bytecode.emit_constant_ref(signature_idx);

    // emits chunk's code attribute
    let segments = compile_code(chunk, exports, bytecode, ctx, cp, options);

    let line_provider = options.line_provider;
    let attribute_count = line_provider.map_or(0, |_| 1);
    bytecode.emit_byte(attribute_count);
}

fn compile_line_mapping_attribute(
    positions: Vec<InstructionPos>,
    content_id: ContentId,
    bytecode: &mut Bytecode,
    line_provider: &dyn SourceLineProvider,
) {
    bytecode.emit_byte(MAPPINGS_ATTRIBUTE);
    let mut mappings: Vec<(usize, u32)> = Vec::new();

    let positions: Vec<_> = positions
        .into_iter()
        .map(|p| (p.source_code_byte_pos, p.instruction))
        .collect();

    let Some(((first_pos, first_ip), positions)) = positions.split_first() else {
        bytecode.emit_u32(0);
        return;
    };
    let mut last_pos = *first_pos;
    let mut last_ip = *first_ip;

    let mut last_line = usize::MAX;
    for (pos, instruction) in positions.iter().copied() {
        if instruction > last_ip {
            let line = line_provider.get_line(content_id, last_pos).unwrap();
            if last_line != line {
                mappings.push((line, last_ip));
            }
            last_line = line;
            last_ip = instruction;
            last_pos = pos;
            continue;
        }
        last_pos = pos;
    }

    if mappings.is_empty() && last_pos != 0 && *first_pos != 0 {
        // if no mappings are set, bind first pos' line with first instruction.
        let line = line_provider.get_line(content_id, *first_pos).unwrap();
        mappings.push((line, 0))
    }

    bytecode.emit_u32(mappings.len() as u32);
    for (line, instruction) in mappings {
        bytecode.emit_u32(instruction);
        bytecode.emit_u32(line as u32);
    }
}

/// compiles chunk's code attribute
/// the code attribute of a chunk is a special attribute that contains the bytecode instructions and
/// locals specifications
///
/// returns the page length (if chunk is a script) and the hir's segments associated with their first instruction.
fn compile_code(
    chunk: &Chunk,
    exports: &NamedExports,
    bytecode: &mut Bytecode,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    options: &CompilerOptions,
) -> Vec<InstructionPos> {
    let locals_byte_count = bytecode.emit_u32_placeholder();
    let (parameters, return_bytes_count) = if let Some(function_id) = chunk.function {
        let function = &ctx.registry[function_id];
        let return_bytes_count = get_type_stack_size(function.return_type) as u8;
        (function.param_types.as_slice(), return_bytes_count)
    } else {
        ([].as_slice(), 0u8)
    };
    let parameters_bytes_count = parameters
        .iter()
        .map(|p| get_type_stack_size(p.ty) as u8 as u32)
        .sum();
    bytecode.emit_u32(parameters_bytes_count);
    bytecode.emit_byte(return_bytes_count);

    // emit instruction count placeholder
    let instruction_count = bytecode.emit_u32_placeholder();
    let mut instructions = Instructions::wrap(bytecode);
    let mut locals = LocalsLayout::new(&chunk.locals);

    let use_value = return_bytes_count != 0 || options.last_page_storage_var.is_some();
    let mut state = EmissionState {
        use_values: use_value,
        ..EmissionState::default()
    };
    emit(
        &chunk.expr,
        &mut instructions,
        ctx,
        cp,
        &mut locals,
        &mut state,
    );
    if let Some(storage_exported_val) = &options.last_page_storage_var {
        let last_expr = if let ExprKind::Block(block) = &chunk.expr.kind {
            block.last().unwrap_or(&chunk.expr)
        } else {
            &chunk.expr
        };
        let page_offset = cp.exported.last().map_or(0, |exp| {
            exp.page_offset + u8::from(ValueStackSize::QWord) as u32
        });
        cp.insert_exported(storage_exported_val, page_offset, last_expr.ty.is_obj());
        instructions.emit_set_external(
            cp.get_external(storage_exported_val).unwrap(),
            last_expr.ty.into(),
        );
    }

    // patch instruction count placeholder
    let instruction_byte_count = instructions.current_ip();
    let segments = instructions.take_positions();
    bytecode.patch_u32_placeholder(instruction_count, instruction_byte_count);

    let locals_length = locals.byte_count();
    bytecode.patch_u32_placeholder(locals_byte_count, locals_length);

    let offsets = locals.refs_offset();
    bytecode.emit_u32(offsets.len() as u32);
    for offset in offsets {
        bytecode.emit_u32(offset)
    }

    segments
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

    writer.write_all(
        &u32::try_from(cp.dynsym.len())
            .expect("dynsym list too large")
            .to_be_bytes(),
    )?;
    for dynsym in &cp.dynsym {
        writer.write_all(&dynsym.to_be_bytes())?;
    }
    Ok(())
}

fn write_exported(
    pool: &mut ConstantPool,
    page_size: u32,
    bytecode: &mut Bytecode,
) -> Result<(), io::Error> {
    bytecode.emit_u32(page_size);
    bytecode.emit_u32(u32::try_from(pool.exported.len()).expect("too many exported vars"));
    for symbol in &pool.exported {
        bytecode.emit_u32(symbol.name_index);
        bytecode.emit_u32(symbol.page_offset);
        bytecode.emit_byte(symbol.is_obj_ref as u8);
    }
    pool.exported.clear();
    Ok(())
}
