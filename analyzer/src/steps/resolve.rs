use crate::engine::Engine;
use crate::environment::Environment;
use crate::name::Name;
use crate::relations::{ResolvedSymbol, Relations, SourceObjectId, UnresolvedImport, UnresolvedImports, Symbol, GlobalObjectId};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use ast::r#use::Import as ImportExpr;
use crate::diagnostic::{Diagnostic, ErrorID, Observation};

/// Used by the resolve step to store resolved imports of an environment.
#[derive(Default, PartialEq)]
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

/// Main structure of the Symbols Resolver
/// The symbol resolver resolves the given relations between the collected symbols in the Engine.
///
/// - lifetime 'a is the structure's lifetime
/// - lifetime 'e is the expressions' lifetime, the Engine and Relations needed a special lifetime
///   as both of them contains references to AST expressions.
struct SymbolResolver<'a, 'e> {
    engine: &'a Engine<'e>,
    diagnostics: Vec<Diagnostic>,
    relations: &'a mut Relations<'e>,
}

impl<'a, 'e> SymbolResolver<'a, 'e> {
    ///Attempts to resolve the unresolved Engine's symbols contained in the given Relations.
    /// Returns a vector of diagnostics if the resolution did reported at least one diagnostic.
    pub fn resolve_symbols(engine: &'a Engine<'e>,
                           relations: &'a mut Relations<'e>) -> Result<(), Vec<Diagnostic>> {
        let mut resolver = Self::new(engine, relations);
        resolver.resolve();
        if resolver.diagnostics.is_empty() {
            return Ok(())
        }
        Err(resolver.diagnostics)
    }

    fn new(engine: &'a Engine<'e>, relations: &'a mut Relations<'e>) -> Self {
        Self {
            engine,
            relations,
            diagnostics: Vec::new(),
        }
    }

    /// The starting point of the resolution phase.
    /// enables the resolution and pushes diagnostics if any symbol could not be resolved.
    fn resolve(&mut self) {
        let mut unresolved_imports = self.relations.take_imports();

        for (env_id, env) in self.engine.environments() {
            let unresolved_imports = unresolved_imports.remove(&env_id)
                .unwrap_or_default();

            let resolved_imports = self.resolve_imports(env_id, unresolved_imports);
            self.resolve_symbols_of(env_id, env, resolved_imports);
        }
    }

    /// resolves the symbols of given environment, using given resolved imports for external symbol references.
    fn resolve_symbols_of(&mut self, env_id: SourceObjectId, env: &Environment, imports: ResolvedImports) {
        for (symbol_name, external_var) in env.variables.external_vars() {
            let object = &mut self.relations.objects[external_var.0];

            match imports.imported_symbols.get(symbol_name) {
                Some(resolved_symbol) => object.resolved = Some(*resolved_symbol),
                None => self.diagnose_unresolved_external_symbols(*external_var, env_id, env, &symbol_name)
            };
        }
    }

    /// Appends a diagnostic for an external symbol that could not be resolved.
    /// Each expression that used this symbol (such as VarReferences) will then get an observation
    fn diagnose_unresolved_external_symbols(&mut self,
                                            external_var: GlobalObjectId,
                                            env_id: SourceObjectId,
                                            env: &Environment,
                                            name: &str) {
        let mut diagnostic = Diagnostic::error(ErrorID::UnknownSymbol, env_id, &format!("Could not resolve symbol {}.", name));

        let observations = env
            .list_annotations()
            .filter(|(_, sym)| match sym {
                Symbol::Local(_) => false,
                Symbol::Global(g) => g == &external_var.0
            })
            .map(|(seg, _)| Observation { segment: seg.clone(), help: None });

        diagnostic.observations = observations.collect();
        diagnostic.observations.sort_by_key(|seg| seg.segment.start);

        self.diagnostics.push(diagnostic);
    }

    /// Appends a diagnostic for an import that could not be resolved.
    /// Each `use` expressions that was referring to the unknown import will get a diagnostic
    fn diagnose_unresolved_import(&mut self,
                                  env_id: SourceObjectId,
                                  imported_symbol_name: Name,
                                  known_parent: Option<Name>,
                                  dependent_expr: &'e ImportExpr<'e>) {
        let msg = &format!(
            "unable to find imported symbol {}{}.",
            known_parent.as_ref().and_then(|p| imported_symbol_name.relative_to(p)).unwrap_or(imported_symbol_name),
            known_parent.map(|p| format!(" in module {p}")).unwrap_or_default()
        );

        let diagnostic = Diagnostic::error(ErrorID::ImportResolution, env_id, msg)
            .with_observation(Observation::new(dependent_expr));
        self.diagnostics.push(diagnostic)
    }

    /// Attempts to resolve all given unresolved imports, returning a [ResolvedImports] structure containing the
    /// imports that could get resolved.
    /// This method will append a new diagnostic for each imports that could not be resolved.
    fn resolve_imports(&mut self, env_id: SourceObjectId, imports: UnresolvedImports<'e>) -> ResolvedImports {
        let mut resolved_imports = ResolvedImports::default();

        //iterate over our unresolved imports
        for (unresolved, dependent) in imports.imports {
            match unresolved {
                // If the unresolved import is a symbol
                UnresolvedImport::Symbol { alias, fqn: name } => {
                    // try to get referenced environment of the import
                    match self.get_env_from(&name) {
                        // if the environment wasn't found, push a diagnostic
                        None => {
                            self.diagnose_unresolved_import(env_id, name, None, dependent)
                        },
                        //else, try to resolve it
                        Some((found_env_id, found_env)) => {
                            let symbol_name = name.simple_name().to_string();
                            let symbol_id = found_env
                                .variables
                                .exported_vars()
                                .position(|var| var.name == symbol_name);

                            if let Some(symbol_id) = symbol_id {
                                let resolved = ResolvedSymbol::new(found_env_id, symbol_id);
                                resolved_imports.set_import(alias.unwrap_or(symbol_name), resolved);
                                continue;
                            }
                            //if the symbol inside the resolved environment could not be found,
                            self.diagnose_unresolved_import(env_id, name, Some(found_env.fqn.clone()), dependent)
                        }
                    }
                },

                //if the unreseloved import is an 'AllIn' import, meaning that it imports all symbols from given module
                UnresolvedImport::AllIn(name) => {
                    // try to get referenced environment of the import
                    match self.get_env_from(&name) {
                        // if the environment wasn't found, push a diagnostic
                        None => {
                            self.diagnose_unresolved_import(env_id, name, None, dependent)
                        },
                        Some((env_id, env)) => {
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
            }
        }
        resolved_imports
    }

    /// Gets an environment from the given fully qualified name, then pop the name until it finds a valid environment.
    /// returns None if the name's root could not get resolved
    fn get_env_from(&self, name: &Name) -> Option<(SourceObjectId, &'a Environment)> {
        let mut env_name = Some(name.clone());
        while let Some(name) = env_name {
            if let Some((id, env)) = self.engine.find_environment_by_name(&name) {
                return Some((id, env));
            }
            env_name = name.tail();
        }
        None
    }
}


#[cfg(test)]
mod tests {
    use crate::engine::Engine;
    use crate::importer::StaticImporter;
    use crate::name::Name;
    use crate::relations::{
        Object, ResolvedSymbol, Relations, SourceObjectId, UnresolvedImport, UnresolvedImports,
    };
    use context::source::{Source, StaticSegmentHolder};
    use pretty_assertions::assert_eq;
    use std::collections::HashMap;
    use indexmap::IndexMap;
    use ast::r#use::Import::AllIn;
    use ast::r#use::ImportedSymbol;
    use parser::{parse_trusted};
    use crate::diagnostic::{Diagnostic, ErrorID, Observation};
    use crate::steps::collect::SymbolCollector;
    use crate::steps::resolve::{ResolvedImports, SymbolResolver};

    #[test]
    fn test_imports_resolution() {
        let math_ast = Source::unknown("val PI = 3.14");
        let std_ast = Source::unknown("val Foo = 'moshell_std'; val Bar = $Foo");
        let io_ast = Source::unknown("val output = 'OutputStream()'; val input = 'InputStream()'");
        let test_ast = Source::unknown("
            use math::PI
            use std::{Bar, io::*}
        ");

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut importer = StaticImporter::new([
                                                   (Name::new("math"), math_ast),
                                                   (Name::new("std"), std_ast),
                                                   (Name::new("std::io"), io_ast),
                                                   (Name::new("test"), test_ast),
                                               ], parse_trusted);
        SymbolCollector::collect_symbols(&mut engine, &mut relations, Name::new("test"), &mut importer)
            .expect("collect errors");

        assert_eq!(
            relations.imports,
            HashMap::from([(
                SourceObjectId(0),
                UnresolvedImports::new(IndexMap::from([
                    (UnresolvedImport::Symbol {
                        alias: None,
                        fqn: Name::new("math::PI"),
                    }, &ast::r#use::Import::Symbol(ImportedSymbol {
                        path: vec!["math"],
                        name: "PI",
                        alias: None,
                        segment: 17..25,
                    })),
                    (UnresolvedImport::Symbol {
                        alias: None,
                        fqn: Name::new("std::Bar"),
                    }, &ast::r#use::Import::Symbol(ImportedSymbol {
                        path: vec![],
                        name: "Bar",
                        alias: None,
                        segment: 48..51,
                    })),
                    (UnresolvedImport::AllIn(Name::new("std::io")), &AllIn(vec!["io"], 53..58)),
                ]))
            )])
        );

        let unresolved_imports = relations.take_imports().remove(&SourceObjectId(0)).unwrap();
        let mut resolver = SymbolResolver::new(&engine, &mut relations);
        let resolved_imports = resolver.resolve_imports(SourceObjectId(0), unresolved_imports);

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

        SymbolCollector::collect_symbols(&mut engine, &mut relations, Name::new("test"), &mut importer)
            .expect("collect errors");
        SymbolResolver::resolve_symbols(&engine, &mut relations)
            .expect("resolution errors");

        assert_eq!(
            relations.objects,
            vec![
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(1), 0)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(2), 1)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(3), 0)),
            ]
        )
    }


    #[test]
    fn test_unknown_symbols() {
        let a_ast = Source::unknown("val C = 'A'");

        let test_ast = Source::unknown("\
        use A::B
        use B::C
        use C::*

        $a; $a; $a
        $C; $B;
        ");
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut importer = StaticImporter::new([
                                                   (Name::new("test"), test_ast),
                                                   (Name::new("A"), a_ast)
                                               ], parse_trusted);
        SymbolCollector::collect_symbols(&mut engine, &mut relations, Name::new("test"), &mut importer)
            .expect("collect errors");

        let diagnostic = SymbolResolver::resolve_symbols(&engine, &mut relations).expect_err("resolution has no errors");

        assert_eq!(
            diagnostic,
            vec![
                Diagnostic::error(ErrorID::ImportResolution, SourceObjectId(0), "unable to find imported symbol B in module A.")
                    .with_observation(Observation::new(&StaticSegmentHolder::new(4..8))),
                Diagnostic::error(ErrorID::ImportResolution, SourceObjectId(0), "unable to find imported symbol B::C.")
                    .with_observation(Observation::new(&StaticSegmentHolder::new(21..25))),
                Diagnostic::error(ErrorID::ImportResolution, SourceObjectId(0), "unable to find imported symbol C.")
                    .with_observation(Observation::new(&StaticSegmentHolder::new(38..42))),
                Diagnostic::error(ErrorID::UnknownSymbol, SourceObjectId(0), "Could not resolve symbol a.")
                    .with_observation(Observation::new(&StaticSegmentHolder::new(52..54)))
                    .with_observation(Observation::new(&StaticSegmentHolder::new(56..58)))
                    .with_observation(Observation::new(&StaticSegmentHolder::new(60..62))),
                Diagnostic::error(ErrorID::UnknownSymbol, SourceObjectId(0), "Could not resolve symbol C.")
                    .with_observation(Observation::new(&StaticSegmentHolder::new(71..73))),
                Diagnostic::error(ErrorID::UnknownSymbol, SourceObjectId(0), "Could not resolve symbol B.")
                    .with_observation(Observation::new(&StaticSegmentHolder::new(75..77))),
            ]
        )
    }


    #[test]
    fn test_local_unknown_symbols() {
        let source = "\
        $C; $C
        var C = 45
        $a; $a; $a
        $C; $C;
        ";
        let test_ast = Source::unknown(source);
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut importer = StaticImporter::new([
                                                   (Name::new("test"), test_ast),
                                               ], parse_trusted);
        SymbolCollector::collect_symbols(&mut engine, &mut relations, Name::new("test"), &mut importer)
            .expect("collect errors");

        let diagnostic = SymbolResolver::resolve_symbols(&engine, &mut relations).expect_err("resolution has no errors");

        assert_eq!(
            diagnostic,
            vec![
                Diagnostic::error(ErrorID::UnknownSymbol, SourceObjectId(0), "Could not resolve symbol C.")
                    .with_observation(Observation::new(&StaticSegmentHolder::new(0..2)))
                    .with_observation(Observation::new(&StaticSegmentHolder::new(4..6))),
                Diagnostic::error(ErrorID::UnknownSymbol, SourceObjectId(0), "Could not resolve symbol a.")
                    .with_observation(Observation::new(&StaticSegmentHolder::new(34..36)))
                    .with_observation(Observation::new(&StaticSegmentHolder::new(38..40)))
                    .with_observation(Observation::new(&StaticSegmentHolder::new(42..44))),
            ]
        )
    }
}