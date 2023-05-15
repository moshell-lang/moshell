use crate::engine::Engine;
use crate::environment::Environment;
use crate::name::Name;
use crate::relations::{
    ResolvedSymbol, Relations, SourceObjectId, UnresolvedImport, UnresolvedImports,
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

struct SymbolResolver<'a> {
    engine: &'a Engine<'a>,
    diagnostics: Vec<Diagnostic>,
    relations: &'a mut Relations,
}

impl<'a> SymbolResolver<'a> {
    ///Attempts to resolve the unresolved symbols contained in the Resolver.
    pub fn resolve_symbols(engine: &'a Engine, relations: &'a mut Relations) -> Result<(), Vec<Diagnostic>> {
        let mut resolver = Self::new(engine, relations);
        resolver.start();
        if resolver.diagnostics.is_empty() {
            return Ok(())
        }
        Err(resolver.diagnostics)
    }

    fn new(engine: &'a Engine, relations: &'a mut Relations) -> Self {
        Self {
            engine,
            relations,
            diagnostics: Vec::new(),
        }
    }

    fn start(&mut self) {
        let mut unresolved_imports = self.relations.take_imports();

        for (env_id, env) in self.engine.environments() {
            let unresolved_imports = unresolved_imports.remove(&env_id)
                .unwrap_or_default();

            let resolved_imports = self.resolve_imports(unresolved_imports);
            self.resolve_symbols_of(env, resolved_imports)
        }
    }


    /// resolves the symbols of an environment
    fn resolve_symbols_of(&mut self, env: &Environment, imports: ResolvedImports) {
        for (symbol_name, resolver_pos) in env.variables.global_vars() {
            let object = &mut self.relations.objects[resolver_pos.0];

            match imports.imported_symbols.get(symbol_name) {
                Some(resolved_symbol) => object.resolved = Some(*resolved_symbol),
                None => self.diagnostics.push(
                    Diagnostic::error(ErrorID::UnknownSymbol, &format!("Could not resolve symbol {symbol_name} in {}", env.fqn))
                    //TODO add origin of resolution in Resolver's objects .with_observation(Observation::new())
                )
            };
        }
    }

    ///Finds the
    fn supply_env<F>(&mut self, name: &Name, consumer: F) where F: FnOnce(&mut Self, SourceObjectId, &Environment) {
        match self.get_env_from(&name) {
            Err(parent) => {
                let msg = &format!("unable to find symbol {name} {}.", parent.map_or(String::new(), |p| format!("in module {p}")));
                self.diagnostics.push(Diagnostic::error(ErrorID::ImportResolution, msg));
            },
            Ok((env_id, env)) => consumer(self, env_id, env)
        }
    }

    /// Resolves given imports and return a structure containing all the imported/aliased imports.
    fn resolve_imports(&mut self, imports: UnresolvedImports) -> ResolvedImports {
        let mut resolved_imports = ResolvedImports::default();

        for unresolved in imports.imports {
            match unresolved {
                UnresolvedImport::Symbol { alias, name } => self.supply_env(&name , |s, env_id, env| {
                    let symbol_name = name.simple_name().to_string();
                    let symbol_id = env
                        .variables
                        .exported_vars()
                        .position(|var| var.name == symbol_name);

                    match symbol_id {
                        Some(symbol_id) => {
                            let resolved = ResolvedSymbol::new(env_id, symbol_id);
                            resolved_imports.set_import(alias.unwrap_or(symbol_name), resolved);
                        }
                        None => s.diagnostics.push(Diagnostic::error(ErrorID::UnknownSymbol, &format!("unknown symbol {symbol_name} in module {}", env.fqn)))
                    }
                }),

                UnresolvedImport::AllIn(name) => self.supply_env(&name, |_, env_id, env| {
                    for var in env.variables.exported_vars() {
                        let var_id = env
                            .variables
                            .exported_vars()
                            .position(|v| v.name == var.name)
                            .unwrap();

                        let import_symbol = ResolvedSymbol::new(env_id, var_id);
                        resolved_imports.set_import(var.name.clone(), import_symbol);
                    }
                })
            }
        }
        resolved_imports
    }

    /// Gets an environment from the given fully qualified name, then pop the name until it finds a valid environment.
    ///
    fn get_env_from(&self, name: &Name) -> Result<(SourceObjectId, &'a Environment), Option<Name>> {
        let mut env_name = Some(name.clone());
        while let Some(name) = env_name {
            if let Some(env_id) = self.engine.find_environment_by_name(&name) {
                return Ok((env_id, self.engine.find_environment(env_id).unwrap()));
            }
            env_name = name.tail();
        }
        Err(env_name)
    }
}


#[cfg(test)]
mod tests {
    use crate::engine::Engine;
    use crate::importer::StaticImporter;
    use crate::steps::collect::collect_symbols;
    use crate::name::Name;
    use crate::relations::{
        Object, ResolvedSymbol, Relations, SourceObjectId, UnresolvedImport, UnresolvedImports,
    };
    use context::source::Source;
    use pretty_assertions::assert_eq;
    use std::collections::HashMap;
    use parser::{parse_trusted};
    use crate::steps::resolve::{ResolvedImports, SymbolResolver};

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
        let mut relations = Relations::default();
        let mut importer = StaticImporter::new([
                                                   (Name::new("math"), math_ast),
                                                   (Name::new("std"), std_ast),
                                                   (Name::new("std::io"), io_ast),
                                                   (Name::new("test"), test_ast),
                                               ], parse_trusted);
        collect_symbols(&mut engine, &mut relations, Name::new("test"), &mut importer)
            .expect("collect errors");

        assert_eq!(
            relations.imports,
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

        let unresolved_imports = relations.take_imports().remove(&SourceObjectId(0)).unwrap();
        let mut resolver = SymbolResolver::new(&engine, &mut relations);
        let resolved_imports = resolver.resolve_imports(unresolved_imports);

        assert_eq!(
            resolved_imports,
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
        );
        assert_eq!(
            resolver.diagnostics,
            vec![]
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
        let mut relations = Relations::default();
        let mut importer = StaticImporter::new([
                                                   (Name::new("math"), math_ast),
                                                   (Name::new("std"), std_ast),
                                                   (Name::new("std::io"), io_ast),
                                                   (Name::new("test"), test_ast),
                                               ], parse_trusted);

        collect_symbols(&mut engine, &mut relations, Name::new("test"), &mut importer)
            .expect("collect errors");

        SymbolResolver::resolve_symbols(&engine, &mut relations).expect("resolution errors");

        assert_eq!(
            relations.objects,
            vec![
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(1), 0)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(2), 1)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(3), 0)),
            ]
        )
    }
}