use std::collections::HashSet;
use std::io;
use std::io::Write;

use analyzer::engine::Engine;
use analyzer::environment::symbols::SymbolInfo;
use analyzer::name::Name;
use analyzer::reef::{Externals, ReefId};
use analyzer::relations::{LocalId, Relations, ResolvedSymbol, SourceId};
use analyzer::types::engine::{Chunk, TypedEngine};
use analyzer::types::hir::ExprKind;
use analyzer::types::Typing;
use context::source::ContentId;

use crate::bytecode::{Bytecode, InstructionPos, Instructions};
use crate::constant_pool::ConstantPool;
use crate::emit::{emit, EmissionState, EmitterContext};
use crate::locals::LocalsLayout;
use crate::r#type::{get_type_stack_size, ValueStackSize};

pub mod bytecode;
mod constant_pool;
mod emit;
mod locals;
mod r#type;

pub(crate) type Captures = Vec<Option<Vec<ResolvedSymbol>>>;

pub trait SourceLineProvider {
    /// returns the line, starting from one, attributed to the given byte position of given content.
    fn get_line(&self, content: ContentId, byte_pos: usize) -> Option<usize>;
}

#[derive(Default)]
pub struct CompilerOptions<'a> {
    pub line_provider: Option<&'a dyn SourceLineProvider>,
    pub last_page_storage_var: Option<String>,
}

const MAPPINGS_ATTRIBUTE: u8 = 1;

#[allow(clippy::too_many_arguments)]
pub fn compile(
    typed_engine: &TypedEngine,
    typing: &Typing,
    relations: &Relations,
    link_engine: &Engine,
    externals: &Externals,
    reef_id: ReefId,
    starting_page: SourceId,
    writer: &mut impl Write,
    options: CompilerOptions,
) -> Result<(), io::Error> {
    let captures = resolve_captures(link_engine, relations, reef_id);

    let mut bytecode = Bytecode::default();
    let mut cp = ConstantPool::default();

    let mut it = typed_engine.group_by_content(link_engine, starting_page);
    while let Some(content) = it.next() {
        let (chunk_id, main_env, main_chunk) = content.main_chunk(&it);
        let ctx = EmitterContext::new(
            reef_id,
            link_engine,
            externals,
            typing,
            main_env,
            &captures,
            chunk_id,
        );
        let page_size = compile_chunk(
            &main_env.fqn,
            main_chunk,
            chunk_id,
            &ctx,
            &mut bytecode,
            &mut cp,
            &options,
        )
        .unwrap();
        write_exported(&mut cp, page_size, &mut bytecode)?;

        // filter out native functions
        let defined_functions: Vec<_> = content.defined_chunks(&it).collect();

        bytecode.emit_u32(defined_functions.len() as u32);

        for (chunk_id, env, chunk) in defined_functions {
            let ctx = EmitterContext::new(
                reef_id,
                link_engine,
                externals,
                typing,
                env,
                &captures,
                chunk_id,
            );

            compile_chunk(
                &env.fqn,
                chunk,
                chunk_id,
                &ctx,
                &mut bytecode,
                &mut cp,
                &options,
            );
        }
    }

    write(writer, &bytecode, &cp)
}

fn compile_chunk(
    name: &Name,
    chunk: &Chunk,
    id: SourceId,
    ctx: &EmitterContext,
    bytecode: &mut Bytecode,
    cp: &mut ConstantPool,
    options: &CompilerOptions,
) -> Option<u32> {
    // emit the function's name
    let signature_idx = cp.insert_string(name);
    bytecode.emit_constant_ref(signature_idx);

    // emits chunk's code attribute
    let (page_size, segments) = compile_chunk_code(chunk, id, bytecode, ctx, cp, options);

    let line_provider = options.line_provider;
    let attribute_count = line_provider.map_or(0, |_| 1);
    bytecode.emit_byte(attribute_count);

    if let Some(line_provider) = line_provider {
        let content = ctx.engine().get_original_content(id);

        let Some(content_id) = content else {
            return page_size;
        };
        compile_line_mapping_attribute(segments, content_id, bytecode, line_provider);
    }
    page_size
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

/// Resolves all captured variables of a given chunk identifier.
///
/// This function will resolve all direct captures of the chunk and the captures of its inner chunks.
/// All resolved captures are set into the given `captures` vector.
fn resolve_captures(engine: &Engine, relations: &Relations, compiled_reef: ReefId) -> Captures {
    let mut externals = HashSet::new();
    let mut captures = vec![None; engine.len()];

    fn resolve(
        chunk_id: SourceId,
        compiled_reef: ReefId,
        engine: &Engine,
        relations: &Relations,
        captures: &mut Vec<Option<Vec<ResolvedSymbol>>>,
        externals: &mut HashSet<ResolvedSymbol>,
    ) {
        let env = engine.get_environment(chunk_id).unwrap();

        // recursively resolve all inner functions
        for func_id in env.iter_direct_inner_environments() {
            resolve(
                func_id,
                compiled_reef,
                engine,
                relations,
                captures,
                externals,
            );
            // filter out external symbols that refers to the current chunk
            externals.retain(|symbol| symbol.source != chunk_id);
        }

        // add this function's external referenced variables
        externals.extend(
            env.symbols
                .external_symbols()
                .map(|(_, relation)| {
                    relations[relation]
                        .state
                        .expect_resolved("unresolved relation during compilation")
                })
                .filter(|symbol| {
                    symbol.reef == compiled_reef && {
                        // filter out functions
                        let env = engine.get_environment(symbol.source).unwrap();
                        let var = env.symbols.get(symbol.object_id).unwrap();
                        var.ty == SymbolInfo::Variable && !(env.is_script && var.is_exported())
                    }
                }),
        );

        let mut chunk_captures: Vec<ResolvedSymbol> = externals.iter().copied().collect();

        chunk_captures.sort_by(|a, b| {
            a.source
                .0
                .cmp(&b.source.0)
                .then_with(|| a.object_id.0.cmp(&b.object_id.0))
        });

        captures[chunk_id.0] = Some(chunk_captures)
    }

    // Resolve captures of all environments, starting from the roots of each module
    for (engine_id, _) in engine.environments().filter(|(_, chunk)| chunk.is_script) {
        resolve(
            engine_id,
            compiled_reef,
            engine,
            relations,
            &mut captures,
            &mut externals,
        );
    }
    captures
}

/// compiles chunk's code attribute
/// the code attribute of a chunk is a special attribute that contains the bytecode instructions and
/// locals specifications
///
/// returns the page length (if chunk is a script) and the hir's segments associated with their first instruction.
fn compile_chunk_code(
    chunk: &Chunk,
    chunk_id: SourceId,
    bytecode: &mut Bytecode,
    ctx: &EmitterContext,
    cp: &mut ConstantPool,
    options: &CompilerOptions,
) -> (Option<u32>, Vec<InstructionPos>) {
    let chunk_expression = chunk
        .expression
        .as_ref()
        .expect("Unable to compile a function without a definition");

    let locals_byte_count = bytecode.emit_u32_placeholder();

    let chunk_captures = ctx.captures[chunk_id.0]
        .as_ref()
        .expect("unresolved capture after resolution");

    // compute the chunk's parameters bytes length
    let parameters_bytes_count: u32 = {
        let explicit_params_count: u32 = chunk
            .parameters
            .iter()
            .map(|p| Into::<u8>::into(get_type_stack_size(p.ty)) as u32)
            .sum::<u32>();
        let captures_params_count: u32 =
            chunk_captures.len() as u32 * u8::from(ValueStackSize::QWord) as u32;
        explicit_params_count + captures_params_count
    };

    bytecode.emit_u32(parameters_bytes_count);
    // emit the function's return bytes count
    let return_bytes_count: u8 = get_type_stack_size(chunk.return_type).into();
    bytecode.emit_byte(return_bytes_count);

    let use_value = return_bytes_count != 0 || options.last_page_storage_var.is_some();

    // emit instruction count placeholder
    let instruction_count = bytecode.emit_u32_placeholder();

    let mut instructions = Instructions::wrap(bytecode);
    let mut locals = LocalsLayout::new(ctx.environment.symbols.all().len() + chunk_captures.len());

    // set space for explicit parameters
    for (id, param) in chunk.parameters.iter().enumerate() {
        locals.set_value_space(LocalId(id), param.ty)
    }

    // set space for implicit captures
    for id in chunk_captures {
        locals.init_external_ref_space(*id)
    }

    let mut state = EmissionState {
        use_values: use_value,
        ..EmissionState::default()
    };

    emit(
        chunk_expression,
        &mut instructions,
        ctx,
        cp,
        &mut locals,
        &mut state,
    );

    let chunk_is_script = ctx.environment.is_script;

    if let Some(storage_exported_val) = &options.last_page_storage_var {
        debug_assert!(chunk_is_script); // only script chunks can store their last expression value in a storage export
        let last_expr = if let ExprKind::Block(b) = &chunk_expression.kind {
            b.last().unwrap_or(chunk_expression)
        } else {
            chunk_expression
        };

        let page_offset = cp.exported.last().map_or(0, |exp| {
            exp.page_offset + u8::from(ValueStackSize::QWord) as u32
        });
        cp.insert_exported(storage_exported_val, page_offset, last_expr.ty.is_obj());
        instructions.emit_set_external(
            cp.get_external(storage_exported_val).unwrap(),
            last_expr.ty.into(),
        )
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

    if !chunk_is_script {
        return (None, segments);
    }

    let mut page_length = locals_length;
    if options.last_page_storage_var.is_some() {
        page_length += u8::from(ValueStackSize::QWord) as u32
    }
    (Some(page_length), segments)
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

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use analyzer::importer::StaticImporter;
    use analyzer::name::Name;
    use analyzer::reef::{Externals, ReefId};
    use analyzer::relations::{LocalId, ResolvedSymbol, SourceId};
    use context::source::Source;
    use parser::parse_trusted;

    use crate::resolve_captures;

    #[test]
    fn test_inner_functions_captures() {
        let src = "\
        fun foo() = {\
           var i = 0
           var b = 1
           fun foo1(n: Int) = {
              fun foo2() = {
                 echo $n $i
              }
              echo $b
           }
           fun bar() = {
             fun bar1() = {
                fun bar2() = {
                   $i
                }
             }
           }
        }\
        ";
        let externals = Externals::default();
        let reef_id = ReefId(1);
        let analyzer = analyzer::analyze(
            Name::new("test"),
            &mut StaticImporter::new([(Name::new("test"), Source::unknown(src))], parse_trusted),
            &externals,
        );
        let captures = resolve_captures(
            &analyzer.resolution.engine,
            &analyzer.resolution.relations,
            reef_id,
        );

        assert_eq!(
            captures,
            vec![
                Some(vec![]), //root
                Some(vec![]), //foo
                Some(vec![
                    //foo1
                    ResolvedSymbol::new(reef_id, SourceId(1), LocalId(0)),
                    ResolvedSymbol::new(reef_id, SourceId(1), LocalId(1)),
                ]),
                Some(vec![
                    //foo2
                    ResolvedSymbol::new(reef_id, SourceId(1), LocalId(0)),
                    ResolvedSymbol::new(reef_id, SourceId(2), LocalId(0)),
                ]),
                Some(vec![
                    //bar
                    ResolvedSymbol::new(reef_id, SourceId(1), LocalId(0)),
                ]),
                Some(vec![
                    //bar1
                    ResolvedSymbol::new(reef_id, SourceId(1), LocalId(0)),
                ]),
                Some(vec![
                    //bar2
                    ResolvedSymbol::new(reef_id, SourceId(1), LocalId(0)),
                ]),
            ]
        )
    }
}
