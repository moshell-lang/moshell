use indexmap::map::Entry;
use indexmap::IndexMap;
use std::collections::HashSet;
use std::io;
use std::io::Write;

use analyzer::engine::Engine;
use analyzer::environment::variables::TypeInfo;
use analyzer::name::Name;
use analyzer::relations::{LocalId, Relations, ResolvedSymbol, SourceId};
use analyzer::types::engine::{Chunk, TypedEngine};
use context::source::ContentId;

use crate::bytecode::{Bytecode, Instructions};
use crate::constant_pool::{ConstantPool, ExportedSymbol};
use crate::emit::{emit, EmissionState, EmitterContext};
use crate::locals::LocalsLayout;
use crate::r#type::{get_type_stack_size, ValueStackSize};

pub mod bytecode;
mod constant_pool;
mod emit;
mod locals;
mod r#type;

pub type Captures = Vec<Option<Vec<ResolvedSymbol>>>;

pub trait SourceLineProvider {
    /// returns the line, starting from one, attributed to the given byte position of given content.
    fn get_line(&self, content: ContentId, byte_pos: usize) -> Option<usize>;
}

pub fn compile(
    typed_engine: &TypedEngine,
    link_engine: &Engine,
    relations: &Relations,
    writer: &mut impl Write,
    line_provider: Option<&dyn SourceLineProvider>,
) -> Result<(), io::Error> {
    let captures = resolve_captures(link_engine, relations, typed_engine);
    let mut bytecode = Bytecode::default();
    let mut cp = ConstantPool::default();

    let mut it = typed_engine.group_by_content(link_engine);
    while let Some(content) = it.next() {
        {
            let (chunk_id, main_env, main_chunk) = content.main_chunk(&it);
            let ctx = EmitterContext {
                environment: main_env,
                engine: link_engine,
                captures: &captures,
                chunk_id,
                is_script: true,
            };
            let name = main_env.fqn.clone();
            compile_chunk(
                name,
                main_chunk,
                chunk_id,
                ctx,
                &mut bytecode,
                &mut cp,
                line_provider,
            );
            write_exported(&mut cp, &mut bytecode)?;
        }
        bytecode.emit_u32(content.function_count() as u32);
        for (chunk_id, env, chunk) in content.function_chunks(&it) {
            let ctx = EmitterContext {
                environment: env,
                engine: link_engine,
                captures: &captures,
                chunk_id,
                is_script: false,
            };
            let name = env.fqn.clone();
            compile_chunk(
                name,
                chunk,
                chunk_id,
                ctx,
                &mut bytecode,
                &mut cp,
                line_provider,
            );
        }
    }

    write(writer, &bytecode, &cp)
}

fn compile_chunk(
    name: Name,
    chunk: &Chunk,
    id: SourceId,
    ctx: EmitterContext,
    bytecode: &mut Bytecode,
    cp: &mut ConstantPool,
    line_provider: Option<&dyn SourceLineProvider>,
) {
    // emit the function's name
    let signature_idx = cp.insert_string(name);
    bytecode.emit_constant_ref(signature_idx);

    let attribute_count_ph = bytecode.emit_u32_placeholder();
    let mut attribute_count = 1;

    // emits chunk's code attribute
    let segments = compile_chunk_code_attribute(chunk, id, bytecode, ctx, cp);

    if let Some(line_provider) = line_provider {
        let content_id = ctx.engine.get_original_content(id).unwrap();
        compile_line_mapping_attribute(segments, content_id, bytecode, line_provider);
        attribute_count += 1
    }

    bytecode.patch_u32_placeholder(attribute_count_ph, attribute_count);
}

fn compile_line_mapping_attribute(
    positions: Vec<(usize, u32)>,
    content_id: ContentId,
    bytecode: &mut Bytecode,
    line_provider: &dyn SourceLineProvider,
) {
    // 2 is the mappings attribute identifier
    bytecode.emit_byte(2);

    let mut mappings = IndexMap::new();

    for (pos, instruction) in positions {
        let line = line_provider.get_line(content_id, pos).unwrap() as u32;

        match mappings.entry(line) {
            Entry::Vacant(v) => {
                v.insert(instruction);
            }
            Entry::Occupied(mut o) => {
                if instruction < *o.get() {
                    o.insert(instruction);
                }
            }
        };
    }

    bytecode.emit_u32(mappings.len() as u32);
    for (line, instruction) in mappings {
        bytecode.emit_u32(instruction);
        bytecode.emit_u32(line);
    }
}

/// Resolves all captured variables of a given chunk identifier.
///
/// This function will resolve all direct captures of the chunk and the captures of its inner chunks.
/// All resolved captures are set into the given `captures` vector.
fn resolve_captures(
    engine: &Engine,
    relations: &Relations,
    typed_engine: &TypedEngine,
) -> Captures {
    let mut externals = HashSet::new();
    let mut captures = vec![None; engine.len()];

    fn resolve(
        chunk_id: SourceId,
        engine: &Engine,
        relations: &Relations,
        captures: &mut Captures,
        externals: &mut HashSet<ResolvedSymbol>,
        is_script: bool,
    ) {
        let env = engine.get_environment(chunk_id).unwrap();

        // recursively resolve all inner functions
        for func_id in env.iter_direct_inner_environments() {
            resolve(func_id, engine, relations, captures, externals, false);
            // filter out external symbols that refers to the current chunk
            externals.retain(|symbol| symbol.source != chunk_id);
        }

        // add this function's external referenced variables
        externals.extend(
            env.variables
                .external_vars()
                .map(|(_, relation)| {
                    relations[relation]
                        .state
                        .expect_resolved("unresolved relation during compilation")
                })
                .filter(|symbol| {
                    // filter out functions
                    let env = engine.get_environment(symbol.source).unwrap();
                    let var = env.variables.get_var(symbol.object_id).unwrap();
                    var.ty == TypeInfo::Variable && !(is_script && var.is_exported())
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

    // resolve capture of all chunks, starting from root chunks of each module
    for (chunk_id, _) in typed_engine
        .iter_chunks()
        .filter(|(_, chunk)| chunk.is_script)
    {
        resolve(
            chunk_id,
            engine,
            relations,
            &mut captures,
            &mut externals,
            true,
        );
    }
    captures
}

/// compiles chunk's code attribute
/// the code attribute of a chunk is a special attribute that contains the bytecode instructions and
/// locals specifications
fn compile_chunk_code_attribute(
    chunk: &Chunk,
    chunk_id: SourceId,
    bytecode: &mut Bytecode,
    ctx: EmitterContext,
    cp: &mut ConstantPool,
) -> Vec<(usize, u32)> {
    // 1 is the code attribute identifier
    bytecode.emit_byte(1);

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

    let use_value = return_bytes_count != 0;

    // emit instruction count placeholder
    let instruction_count = bytecode.emit_u32_placeholder();

    let mut instructions = Instructions::wrap(bytecode);
    let mut locals =
        LocalsLayout::new(ctx.environment.variables.all_vars().len() + chunk_captures.len());

    // set space for explicit parameters
    for (id, param) in chunk.parameters.iter().enumerate() {
        locals.set_value_space(LocalId(id), param.ty.into())
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
        &chunk.expression,
        &mut instructions,
        ctx,
        cp,
        &mut locals,
        &mut state,
    );

    // patch instruction count placeholder
    let instruction_byte_count = instructions.current_ip();
    let segments = instructions.take_positions();
    bytecode.patch_u32_placeholder(instruction_count, instruction_byte_count);

    let locals_length = locals.byte_count();
    bytecode.patch_u32_placeholder(locals_byte_count, locals_length);
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

fn write_exported(pool: &mut ConstantPool, bytecode: &mut Bytecode) -> Result<(), io::Error> {
    bytecode.emit_u32(u32::try_from(pool.exported.len()).expect("too many exported vars"));
    for ExportedSymbol {
        name_index,
        local_offset,
    } in &pool.exported
    {
        bytecode.emit_u32(*name_index);
        bytecode.emit_u32(*local_offset);
    }
    pool.exported.clear();
    Ok(())
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use analyzer::importer::StaticImporter;
    use analyzer::name::Name;
    use analyzer::relations::{LocalId, ResolvedSymbol, SourceId};
    use analyzer::steps::typing::apply_types;
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
        let result = analyzer::resolve_all(
            Name::new("test"),
            &mut StaticImporter::new([(Name::new("test"), Source::unknown(src))], parse_trusted),
        );

        let (typed_engine, _) = apply_types(&result.engine, &result.relations, &mut vec![]);

        let captures = resolve_captures(&result.engine, &result.relations, &typed_engine);

        assert_eq!(
            captures,
            vec![
                Some(vec![]), //root
                Some(vec![]), //foo
                Some(vec![
                    //foo1
                    ResolvedSymbol::new(SourceId(1), LocalId(0)),
                    ResolvedSymbol::new(SourceId(1), LocalId(1)),
                ]),
                Some(vec![
                    //foo2
                    ResolvedSymbol::new(SourceId(1), LocalId(0)),
                    ResolvedSymbol::new(SourceId(2), LocalId(0)),
                ]),
                Some(vec![
                    //bar
                    ResolvedSymbol::new(SourceId(1), LocalId(0)),
                ]),
                Some(vec![
                    //bar1
                    ResolvedSymbol::new(SourceId(1), LocalId(0)),
                ]),
                Some(vec![
                    //bar2
                    ResolvedSymbol::new(SourceId(1), LocalId(0)),
                ]),
            ]
        )
    }
}
