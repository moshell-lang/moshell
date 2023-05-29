use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use context::source::SourceSegment;

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::environment::Environment;
use crate::name::Name;
use crate::relations::{
    GlobalObjectId, Relations, ResolvedSymbol, SourceObjectId, Symbol, UnresolvedImport,
    UnresolvedImports,
};

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
/// - lifetime 'a is the lifetime of references
/// - lifetime 'e is the expressions' lifetime, the Engine and Relations needed a special lifetime
///   as both of them contains references to AST expressions.
pub struct SymbolResolver<'a, 'e> {
    engine: &'a Engine<'e>,
    diagnostics: Vec<Diagnostic>,
    relations: &'a mut Relations,
}

impl<'a, 'e> SymbolResolver<'a, 'e> {
    ///Attempts to resolve the unresolved Engine's symbols contained in the given Relations.
    /// Returns a vector of diagnostics raised by the resolution process.
    pub fn resolve_symbols(
        engine: &'a Engine<'e>,
        relations: &'a mut Relations,
    ) -> Vec<Diagnostic> {
        let mut resolver = Self::new(engine, relations);
        resolver.resolve();
        resolver.diagnostics
    }

    /// Resolves symbols in an immediate environment.
    ///
    /// Nested environments, where [`Environment::has_strict_declaration_order`] is `true`, resolve
    /// differently from non-nested environments. In a nested environment, the symbols captures
    /// the order of the declarations. To know what is in scope, resolution must be done immediately
    /// after the declaration of the environment that captures, during the collection phase.
    ///
    /// Imports are on the other hand always resolved after the collection phase is complete, during
    /// a call to [`SymbolResolver::resolve_trees`], when using [`SymbolResolver::resolve_symbols`].
    pub fn resolve_captures(
        env_stack: &[(SourceObjectId, &Environment)],
        relations: &'a mut Relations,
        capture_env: &Environment,
    ) {
        'capture: for (name, object_id) in capture_env.variables.external_vars() {
            for (module, env) in env_stack.iter().rev() {
                if let Some(Symbol::Local(local)) = env.variables.get_reachable(name) {
                    relations.objects[object_id.0].resolved = Some(ResolvedSymbol {
                        module: *module,
                        object_id: local,
                    });
                    continue 'capture;
                }
            }
        }
    }

    fn new(engine: &'a Engine<'e>, relations: &'a mut Relations) -> Self {
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
        let mut resolved_imports = HashMap::new();
        for (env_id, env) in self.engine.environments() {
            let unresolved_imports = unresolved_imports.remove(&env_id).unwrap_or_default();

            let local_resolved_imports = self.resolve_imports(env_id, unresolved_imports);
            self.resolve_symbols_of(env, &local_resolved_imports);
            resolved_imports.insert(env_id, local_resolved_imports);
        }
        self.resolve_trees(&resolved_imports);
    }

    /// resolves the symbols of given environment, using given resolved imports for external symbol references.
    fn resolve_symbols_of(&mut self, env: &Environment, imports: &ResolvedImports) {
        for (symbol_name, external_var) in env.variables.external_vars() {
            let object = &mut self.relations.objects[external_var.0];

            if let Some(resolved_symbol) = imports.imported_symbols.get(symbol_name) {
                object.resolved = Some(*resolved_symbol);
            };
        }
    }

    /// Appends a diagnostic for an external symbol that could not be resolved.
    ///
    /// Each expression that use this symbol (such as variable references) will then get an observation.
    fn diagnose_unresolved_external_symbols(
        external_var: GlobalObjectId,
        env_id: SourceObjectId,
        env: &Environment,
        name: &str,
    ) -> Diagnostic {
        let mut diagnostic = Diagnostic::new(
            DiagnosticID::UnknownSymbol,
            env_id,
            format!("Could not resolve symbol {name}."),
        );

        let observations = env
            .list_definitions()
            .filter(|(_, sym)| match sym {
                Symbol::Local(_) => false,
                Symbol::Global(g) => g == &external_var.0,
            })
            .map(|(seg, _)| Observation {
                segment: seg.clone(),
                help: None,
            });

        diagnostic.observations = observations.collect();
        diagnostic.observations.sort_by_key(|seg| seg.segment.start);

        diagnostic
    }

    /// Appends a diagnostic for an import that could not be resolved.
    /// Each `use` expressions that was referring to the unknown import will get a diagnostic
    fn diagnose_unresolved_import(
        &mut self,
        env_id: SourceObjectId,
        imported_symbol_name: Name,
        known_parent: Option<Name>,
        dependent_segment: SourceSegment,
    ) -> Diagnostic {
        let msg = format!(
            "unable to find imported symbol {}{}.",
            known_parent
                .as_ref()
                .and_then(|p| imported_symbol_name.relative_to(p))
                .unwrap_or(imported_symbol_name),
            known_parent
                .map(|p| format!(" in module {p}"))
                .unwrap_or_default()
        );

        Diagnostic::new(DiagnosticID::ImportResolution, env_id, msg)
            .with_observation(Observation::new(dependent_segment))
    }

    /// Attempts to resolve all given unresolved imports, returning a [ResolvedImports] structure containing the
    /// imports that could get resolved.
    /// This method will append a new diagnostic for each imports that could not be resolved.
    fn resolve_imports(
        &mut self,
        env_id: SourceObjectId,
        imports: UnresolvedImports,
    ) -> ResolvedImports {
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
                            let diagnostic =
                                self.diagnose_unresolved_import(env_id, name, None, dependent);
                            self.diagnostics.push(diagnostic);
                        }
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
                            let diagnostic = self.diagnose_unresolved_import(
                                env_id,
                                name,
                                Some(found_env.fqn.clone()),
                                dependent,
                            );
                            self.diagnostics.push(diagnostic);
                        }
                    }
                }

                //if the unreseloved import is an 'AllIn' import, meaning that it imports all symbols from given module
                UnresolvedImport::AllIn(name) => {
                    // try to get referenced environment of the import
                    match self.get_env_from(&name) {
                        // if the environment wasn't found, push a diagnostic
                        None => {
                            let diagnostic =
                                self.diagnose_unresolved_import(env_id, name, None, dependent);
                            self.diagnostics.push(diagnostic)
                        }
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

    /// Iterates over remaining unresolved symbols, and tries to resolve them by traversing the parent chain.
    ///
    /// This resolution should happen after all imports have been resolved in their respective environments,
    /// to allow child environments to use imports from their parents.
    fn resolve_trees(&mut self, resolved_imports: &HashMap<SourceObjectId, ResolvedImports>) {
        'symbol: for (object_id, object) in self.relations.iter_mut() {
            if object.resolved.is_some() {
                continue;
            }
            let origin = object.origin;

            // Get the local naming of the object
            let origin_env = self
                .engine
                .get_environment(origin)
                .expect("Environment declared an unknown parent");
            let name = origin_env
                .variables
                .get_symbol_name(object_id)
                .expect("Unknown object name");

            // Go up the parent chain until we find the symbol or we reach the root
            let mut current = origin_env;
            while let Some((module, env)) = current
                .parent
                .and_then(|id| self.engine.get_environment(id).map(|env| (id, env)))
            {
                // Locals symbols are always treated first, before imports.
                // The current environment might already owns the resolution result as a global symbol.
                // This happens only if it used it, so we ignore that fact here to always solve external
                // symbols via imports.
                if !env.has_strict_declaration_order() {
                    if let Some(Symbol::Local(local)) = env.variables.get_exported(name) {
                        object.resolved = Some(ResolvedSymbol {
                            module,
                            object_id: local,
                        });
                        continue 'symbol;
                    }
                }

                // If the symbol is imported, resolve it directly.
                if let Some(resolved_imports) = resolved_imports.get(&module) {
                    if let Some(resolved) = resolved_imports.imported_symbols.get(name) {
                        object.resolved = Some(*resolved);
                        continue 'symbol;
                    }
                }
                current = env;
            }

            // If we reach this point, the symbol could not be resolved, during any of the previous phases.
            self.diagnostics
                .push(Self::diagnose_unresolved_external_symbols(
                    object_id, origin, origin_env, name,
                ));
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use indexmap::IndexMap;
    use pretty_assertions::assert_eq;

    use context::source::Source;
    use context::str_find::{find_in, find_in_nth};
    use parser::parse_trusted;

    use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
    use crate::engine::Engine;
    use crate::importer::StaticImporter;
    use crate::name::Name;
    use crate::relations::{
        Object, Relations, ResolvedSymbol, SourceObjectId, UnresolvedImport, UnresolvedImports,
    };
    use crate::steps::collect::SymbolCollector;
    use crate::steps::resolve::{ResolvedImports, SymbolResolver};

    #[test]
    fn test_imports_resolution() {
        let math_ast = Source::unknown("val PI = 3.14");
        let std_ast = Source::unknown("val Foo = 'moshell_std'; val Bar = $Foo");
        let io_ast = Source::unknown("val output = 'OutputStream()'; val input = 'InputStream()'");
        let test_ast = Source::unknown(
            "
            use math::PI
            use std::{Bar, io::*}
        ",
        );

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut importer = StaticImporter::new(
            [
                (Name::new("math"), math_ast),
                (Name::new("std"), std_ast),
                (Name::new("std::io"), io_ast),
                (Name::new("test"), test_ast),
            ],
            parse_trusted,
        );
        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            Name::new("test"),
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);

        assert_eq!(
            relations.imports,
            HashMap::from([(
                SourceObjectId(0),
                UnresolvedImports::new(IndexMap::from([
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            fqn: Name::new("math::PI"),
                        },
                        17..25
                    ),
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            fqn: Name::new("std::Bar"),
                        },
                        48..51
                    ),
                    (UnresolvedImport::AllIn(Name::new("std::io")), 53..58),
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
        assert_eq!(resolver.diagnostics, vec![])
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
        let mut importer = StaticImporter::new(
            [
                (Name::new("math"), math_ast),
                (Name::new("std"), std_ast),
                (Name::new("std::io"), io_ast),
                (Name::new("test"), test_ast),
            ],
            parse_trusted,
        );

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            Name::new("test"),
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);
        let diagnostics = SymbolResolver::resolve_symbols(&engine, &mut relations);
        assert_eq!(diagnostics, vec![]);

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

        let source = "\
        use A::B
        use B::C
        use C::*

        $a; $a; $a
        $C; $B;
        ";
        let test_ast = Source::unknown(source);
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut importer = StaticImporter::new(
            [(Name::new("test"), test_ast), (Name::new("A"), a_ast)],
            parse_trusted,
        );
        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            Name::new("test"),
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);

        let diagnostics = SymbolResolver::resolve_symbols(&engine, &mut relations);

        assert_eq!(
            diagnostics,
            vec![
                Diagnostic::new(
                    DiagnosticID::ImportResolution,
                    SourceObjectId(0),
                    "unable to find imported symbol B in module A."
                )
                .with_observation(Observation::new(find_in(source, "A::B"))),
                Diagnostic::new(
                    DiagnosticID::ImportResolution,
                    SourceObjectId(0),
                    "unable to find imported symbol B::C."
                )
                .with_observation(Observation::new(find_in(source, "B::C"))),
                Diagnostic::new(
                    DiagnosticID::ImportResolution,
                    SourceObjectId(0),
                    "unable to find imported symbol C."
                )
                .with_observation(Observation::new(find_in(source, "C::*"))),
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    SourceObjectId(0),
                    "Could not resolve symbol a."
                )
                .with_observation(Observation::new(find_in_nth(source, "$a", 0)))
                .with_observation(Observation::new(find_in_nth(source, "$a", 1)))
                .with_observation(Observation::new(find_in_nth(source, "$a", 2))),
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    SourceObjectId(0),
                    "Could not resolve symbol C."
                )
                .with_observation(Observation::new(find_in_nth(source, "$C", 0))),
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    SourceObjectId(0),
                    "Could not resolve symbol B."
                )
                .with_observation(Observation::new(find_in(source, "$B"))),
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
        let mut importer = StaticImporter::new([(Name::new("test"), test_ast)], parse_trusted);
        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            Name::new("test"),
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);

        let diagnostic = SymbolResolver::resolve_symbols(&engine, &mut relations);

        assert_eq!(
            diagnostic,
            vec![
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    SourceObjectId(0),
                    "Could not resolve symbol C."
                )
                .with_observation(Observation::new(find_in(source, "$C")))
                .with_observation(Observation::new(find_in_nth(source, "$C", 1))),
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    SourceObjectId(0),
                    "Could not resolve symbol a."
                )
                .with_observation(Observation::new(find_in_nth(source, "$a", 0)))
                .with_observation(Observation::new(find_in_nth(source, "$a", 1)))
                .with_observation(Observation::new(find_in_nth(source, "$a", 2))),
            ]
        )
    }

    #[test]
    fn find_in_parent_environment() {
        let source = Source::unknown("val found = false; fun find() = $found");

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut importer = StaticImporter::new([(Name::new("test"), source)], parse_trusted);
        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            Name::new("test"),
            &mut importer,
        );

        assert_eq!(diagnostics, vec![]);

        let mut resolver = SymbolResolver::new(&mut engine, &mut relations);
        resolver.resolve();

        assert_eq!(resolver.diagnostics, vec![]);

        assert_eq!(
            relations.objects,
            vec![Object::resolved(
                SourceObjectId(1),
                ResolvedSymbol::new(SourceObjectId(0), 0)
            )]
        )
    }
}
