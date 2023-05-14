use crate::engine::Engine;
use analyzer_system::environment::Environment;
use analyzer_system::name::Name;
use analyzer_system::resolver::{
    ResolvedSymbol, Resolver, SourceObjectId, UnresolvedImport, UnresolvedImports,
};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use crate::diagnostic::{Diagnostic, ErrorID};

/// Used by the resolve step to store resolved imports of an environment.
#[derive(Default, Eq, PartialEq)]
struct ResolvedImports {
    imported_symbols: HashMap<String, ResolvedSymbol>,
}

impl Debug for ResolvedImports {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut imports: Vec<_> = self.imported_symbols.iter().collect();
        imports.sort_by_key(|(k, _)| *k);
        f.debug_struct("ResolvedImports")
            .field("imported_symbols", &imports)
            .finish()
    }
}

impl ResolvedImports {
    fn set_import(&mut self, symbol_name: String, symbol: ResolvedSymbol) {
        self.imported_symbols.insert(symbol_name, symbol);
    }

    fn with(symbols: HashMap<String, ResolvedSymbol>) -> Self {
        Self {
            imported_symbols: symbols,
        }
    }
}

///Attempts to resolve the unresolved symbols contained in the Resolver.
pub fn resolve_symbols(engine: &Engine, resolver: &mut Resolver) -> Result<(), Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();

    let mut unresolved_imports = resolver.take_imports();


    for (env_id, env) in engine.environments() {
        let unresolved_imports = unresolved_imports.remove(&env_id)
            .unwrap_or_default();

        if let Some(resolved_imports) = resolve_imports(engine, unresolved_imports, &mut diagnostics) {
            resolve_symbols_of(env, resolved_imports, resolver, &mut diagnostics);
        }
    }

    Ok(())
}

/// resolves the symbols of an environment
fn resolve_symbols_of(
    env: &Environment,
    imports: ResolvedImports,
    resolver: &mut Resolver,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for (symbol_name, resolver_pos) in env.variables.global_vars() {
        let object = &mut resolver.objects[resolver_pos.0];

        match imports.imported_symbols.get(symbol_name) {
            Some(resolved_symbol) => object.resolved = Some(*resolved_symbol),
            None => diagnostics.push(
                Diagnostic::error(ErrorID::ImportResolution, &format!("Could not resolve symbol {symbol_name} in {}", env.fqn))
                //TODO add origin of resolution in Resolver's objects .with_observation(Observation::new())
            )
        };
    }
}

/// Gets an environment from his fully qualified name,
/// if an environment could not be found
fn get_env_from<'a>(
    name: &Name,
    engine: &'a Engine,
) -> Option<(SourceObjectId, &'a Environment)> {
    let mut env_name = Some(name.clone());
    while let Some(name) = env_name {
        if let Some(env_id) = engine.find_environment_by_name(&name) {
            return Some((env_id, engine.find_environment(env_id).unwrap()));
        }
        env_name = name.tail();
    }
    None
}

/// Resolves given imports and return a structure containing all the imported/aliased imports.
fn resolve_imports(engine: &Engine,
                   imports: UnresolvedImports,
                   diagnostics: &mut Vec<Diagnostic>) -> Option<ResolvedImports> {
    let mut resolved_imports = ResolvedImports::default();

    for unresolved in imports.imports {
        match unresolved {
            UnresolvedImport::Symbol { alias, name } => {
                let (env_id, env) = get_env_from(&name, engine)?;
                let symbol_name = name.simple_name().to_string();
                let symbol_id = env
                    .variables
                    .exported_vars()
                    .position(|var| var.name == symbol_name);

                match symbol_id {
                    Some(symbol_id) => {
                        let resolved = ResolvedSymbol::new(env_id, symbol_id);
                        resolved_imports.set_import(alias.unwrap_or(symbol_name), resolved)
                    }
                    None => diagnostics.push(Diagnostic::error(ErrorID::UnknownSymbol, &format!("unknown symbol {symbol_name} in module {}", env.fqn)))
                }
            }

            UnresolvedImport::AllIn(name) => {
                let (env_id, env) = get_env_from(&name, engine)?;

                for var in env.variables.exported_vars() {
                    let var_id = env
                        .variables
                        .exported_vars()
                        .position(|v| v.name == var.name)
                        .unwrap();

                    let import_symbol = ResolvedSymbol::new(env_id, var_id);
                    resolved_imports.set_import(var.name.clone(), import_symbol);
                }
            }
        }
    }
    Some(resolved_imports)
}

#[cfg(test)]
mod tests {
    use crate::engine::Engine;
    use crate::importer::StaticImporter;
    use crate::steps::collect::collect_symbols;
    use crate::steps::resolve::{resolve_imports, resolve_symbols, ResolvedImports};
    use analyzer_system::name::Name;
    use analyzer_system::resolver::{
        Object, ResolvedSymbol, Resolver, SourceObjectId, UnresolvedImport, UnresolvedImports,
    };
    use context::source::Source;
    use pretty_assertions::assert_eq;
    use std::collections::HashMap;
    use parser::{parse_trusted};

    #[test]
    fn test_imports_resolution() {
        let math_ast = Source::unknown("val PI = 3.14");
        let std_ast = Source::unknown("val Foo = 'moshell_std'; val Bar = $Foo");
        let io_ast = Source::unknown("val output = 'OutputStream()'; val input = 'InputStream()'");
        let test_ast = Source::unknown(
            "\
            use math::PI
            use std::{Bar, io::*}
        ",
        );

        let mut engine = Engine::default();
        let mut resolver = Resolver::default();
        let mut importer = StaticImporter::new([
            (Name::new("math"), math_ast),
            (Name::new("std"), std_ast),
            (Name::new("std::io"), io_ast),
            (Name::new("test"), test_ast),
        ], parse_trusted);
        collect_symbols(&mut engine, &mut resolver, Name::new("test"), &mut importer)
            .expect("collect errors");

        assert_eq!(
            resolver.imports,
            HashMap::from([(
                SourceObjectId(0),
                UnresolvedImports::new(vec![
                    UnresolvedImport::Symbol {
                        alias: None,
                        name: Name::new("math::PI")
                    },
                    UnresolvedImport::Symbol {
                        alias: None,
                        name: Name::new("std::Bar"),
                    },
                    UnresolvedImport::AllIn(Name::new("std::io")),
                ])
            )])
        );

        let unresolved_imports = resolver.take_imports().remove(&SourceObjectId(0)).unwrap();
        let mut diagnostics = vec![];
        let resolved = {
            resolve_imports(&engine, unresolved_imports, &mut diagnostics)
                .expect("resolution errors")
        };

        assert_eq!(
            resolved,
            ResolvedImports::with(HashMap::from([
                ("PI".to_string(), ResolvedSymbol::new(SourceObjectId(3), 0)),
                ("Bar".to_string(), ResolvedSymbol::new(SourceObjectId(2), 1)),
                (
                    "output".to_string(),
                    ResolvedSymbol::new(SourceObjectId(1), 0)
                ),
                (
                    "input".to_string(),
                    ResolvedSymbol::new(SourceObjectId(1), 1)
                ),
            ]))
        )
    }

    #[test]
    fn test_symbols_resolution() {
        let math_ast = Source::unknown("val PI = 3.14");

        let std_ast = Source::unknown("val Foo = 'moshell_std'; val Bar = $Foo");

        let io_ast = Source::unknown("val output = 'OutputStream()'; val input = 'InputStream()'");

        let test_ast = Source::unknown(
            "\
            use math::PI
            use std::{Bar, io::*}

            val output = $output
            val x = $Bar
            val y = $PI
        ",
        );

        let mut engine = Engine::default();
        let mut resolver = Resolver::default();
        let mut importer = StaticImporter::new([
            (Name::new("math"), math_ast),
            (Name::new("std"), std_ast),
            (Name::new("std::io"), io_ast),
            (Name::new("test"), test_ast),
        ], parse_trusted);

        collect_symbols(&mut engine, &mut resolver, Name::new("test"), &mut importer)
            .expect("collect errors");

        resolve_symbols(&engine, &mut resolver).expect("resolution errors");

        assert_eq!(
            resolver.objects,
            vec![
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(1), 0)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(2), 1)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(3), 0)),
            ]
        )
    }
}
