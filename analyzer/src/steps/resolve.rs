use std::collections::HashSet;
use std::iter::once;

use crate::diagnostic::Diagnostic;
use crate::engine::Engine;
use crate::environment::symbols::{resolve_loc, SymbolRegistry};
use crate::environment::Environment;
use crate::imports::Imports;
use crate::name::Name;
use crate::reef::{Externals, ReefId};
use crate::relations::{
    LocalId, RelationId, RelationState, Relations, ResolvedSymbol, SourceId, SymbolRef,
};
use crate::steps::resolve::diagnostics::*;
use crate::steps::resolve::symbol::SymbolResolutionResult;
use crate::steps::shared_diagnostics::diagnose_invalid_symbol;

mod diagnostics;
mod import;
mod symbol;

/// Main structure of the Symbols Resolver
/// The symbol resolver resolves the given relations between the collected symbols in the Engine.
///
/// - lifetime 'a is the lifetime of references
/// - lifetime 'e is the expressions' lifetime, the Engine and Relations needed a special lifetime
///   as both of them contains references to AST expressions.
pub struct SymbolResolver<'a, 'e> {
    engine: &'a Engine<'e>,
    relations: &'a mut Relations,
    imports: &'a mut Imports,
    externals: &'a Externals<'a>,

    diagnostics: Vec<Diagnostic>,
}

impl<'a, 'e> SymbolResolver<'a, 'e> {
    ///Attempts to resolve the unresolved Engine's symbols contained in the given Relations.
    /// Returns a vector of diagnostics raised by the resolution process.
    pub fn resolve_symbols(
        engine: &'a Engine<'e>,
        relations: &'a mut Relations,
        imports: &'a mut Imports,
        externals: &'a Externals<'e>,
        to_visit: &mut Vec<Name>,
        visited: &HashSet<Name>,
    ) -> Vec<Diagnostic> {
        let mut resolver = Self::new(engine, relations, imports, externals);
        resolver.resolve(to_visit, visited);
        resolver.diagnostics
    }

    /// Resolves symbols in an immediate environment,
    /// where the capture environment is the last env of the given `env_stack`
    ///
    /// Nested environments, where [`Environment::has_strict_declaration_order`] is `true`, resolve
    /// differently from non-nested environments. In a nested environment, the symbols captures
    /// the order of the declarations. To know what is in scope, resolution must be done immediately
    /// after the declaration of the environment that captures, during the collection phase.
    ///
    /// Imports are on the other hand always resolved after the collection phase is complete, during
    /// a call to [`SymbolResolver::resolve_trees`], when using [`SymbolResolver::resolve_symbols`].
    pub fn resolve_captures(
        env_stack: &[(SourceId, &Environment)],
        relations: &mut Relations,
        reef: ReefId,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        fn diagnose_invalid_symbol_in_capture(
            env_stack: Vec<&Environment>,
            capture_env_id: SourceId,
            reef: ReefId,
            name: &Name,
            local: LocalId,
            external: RelationId,
        ) -> Diagnostic {
            let mut segments: Vec<_> = env_stack
                .iter()
                .flat_map(|env| env.find_references(SymbolRef::External(external)))
                .collect();

            segments.sort_by_key(|s| s.start);

            //TODO support observations in foreign environments to include concerned symbol declaration in diagnostics
            let declaration_env = *env_stack.last().unwrap();

            let var = declaration_env.symbols.get(local).unwrap();
            diagnose_invalid_symbol(var.ty, capture_env_id, reef, name, &segments)
        }

        let ((capture_env_id, capture_env), parents) =
            env_stack.split_last().expect("env_stack is empty");

        'capture: for (loc, relation_id) in capture_env.symbols.external_symbols() {
            let name = &loc.name;
            for (pos, (env_id, env)) in parents.iter().rev().enumerate() {
                let relation = &mut relations[relation_id];

                if let Some(local) = env.symbols.find_reachable(name.root(), relation.registry) {
                    if name.is_qualified() {
                        let erroneous_capture = parents.iter().rev().take(pos + 1).map(|(_, s)| *s);

                        let erroneous_capture =
                            once(*capture_env).chain(erroneous_capture).collect();

                        let diagnostic = diagnose_invalid_symbol_in_capture(
                            erroneous_capture,
                            *capture_env_id,
                            reef,
                            name,
                            local,
                            relation_id,
                        );
                        diagnostics.push(diagnostic);
                        relation.state = RelationState::Dead;
                    } else {
                        let symbol = ResolvedSymbol {
                            reef,
                            source: *env_id,
                            object_id: local,
                        };
                        relation.state = RelationState::Resolved(symbol);
                    }
                    continue 'capture;
                }
            }
        }
    }

    fn new(
        engine: &'a Engine<'e>,
        relations: &'a mut Relations,
        imports: &'a mut Imports,
        externals: &'a Externals<'e>,
    ) -> Self {
        Self {
            engine,
            relations,
            imports,
            externals,

            diagnostics: Vec::new(),
        }
    }

    /// The starting point of the resolution phase.
    /// enables the resolution and pushes diagnostics if any symbol could not be resolved.
    fn resolve(&mut self, to_visit: &mut Vec<Name>, visited: &HashSet<Name>) {
        for (env_id, _) in self.engine.environments() {
            if let Some(imports) = self.imports.get_imports_mut(env_id) {
                Self::resolve_imports(
                    self.externals,
                    self.engine,
                    env_id,
                    imports,
                    &mut self.diagnostics,
                );
            }
        }
        self.resolve_trees(to_visit, visited);
    }

    /// Iterates over remaining unresolved symbols, and tries to resolve them by traversing the parent chain.
    ///
    /// This resolution should happen after all imports have been resolved in their respective environments,
    /// to allow child environments to use imports from their parents.
    fn resolve_trees(&mut self, to_visit: &mut Vec<Name>, visited: &HashSet<Name>) {
        for (relation_id, object) in self.relations.iter_mut() {
            if object.state != RelationState::Unresolved {
                continue;
            }
            let (origin, registry) = { (object.origin, object.registry) };

            // Get the local naming of the object
            let origin_env = self
                .engine
                .get_environment(origin)
                .expect("Environment declared an unknown parent");

            let symbol_loc = origin_env
                .symbols
                .find_external_symbol_name(relation_id)
                .expect("Unknown object name");
            let symbol_name = &symbol_loc.name;

            let mut result = SymbolResolutionResult::NotFound;

            // if it explicitly targets the current reef, then do a simple search of the corresponding name
            if symbol_loc.is_current_reef_explicit {
                result = Self::resolve_absolute_symbol(
                    self.engine,
                    symbol_name,
                    self.externals.current,
                    registry,
                );
            } else {
                // Follow the parent chain until we get a decisive result
                let mut current = Some((origin, origin_env));
                while let Some((env_id, env)) = current {
                    if env_id != origin {
                        // Locals symbols are always treated first, before imports.
                        // The current environment might already owns the resolution result as a global symbol.
                        // This happens only if it used it, so we ignore that fact here to always solve external
                        // symbols via imports.
                        //
                        // We omit this resolution for origin environments as the resolution of their own locals
                        // is done by the collection phase
                        result = Self::resolve_symbol_from_locals(
                            env_id,
                            env,
                            symbol_name,
                            self.externals.current,
                            registry,
                        );
                    }

                    if result == SymbolResolutionResult::NotFound {
                        if let Some(imports) = &self.imports.get_imports(env_id) {
                            // If the symbol wasn't found from the environment locals, try to resolve using its imports
                            result = Self::resolve_symbol_from_imports(
                                self.engine,
                                imports,
                                symbol_name,
                                self.externals.current,
                                registry,
                            )
                        }
                    }

                    if result != SymbolResolutionResult::NotFound {
                        break; //we found something
                    }

                    current = env
                        .parent
                        .and_then(|id| self.engine.get_environment(id).map(|env| (id, env)));
                }

                //ultimate step is to try to resolve this symbol as an absolute symbol.
                if result == SymbolResolutionResult::NotFound {
                    result = resolve_loc(symbol_loc, self.engine, self.externals)
                        .map(|(engine, id)| {
                            Self::resolve_absolute_symbol(engine, symbol_name, id, registry)
                        })
                        .unwrap_or(SymbolResolutionResult::NotFound)
                }

                //if the symbol is a type, and if its name is unqualified, try to resolve it from the special `lang` reef.
                if registry == SymbolRegistry::Types
                    && (!symbol_name.is_qualified()
                        || (symbol_name.parts().len() == 2 && symbol_name.root() == "lang"))
                {
                    if let Some(primitive_type) = self
                        .externals
                        .lang()
                        .type_context
                        .get_type_id(symbol_name.simple_name())
                    {
                        result = SymbolResolutionResult::Resolved(ResolvedSymbol::lang_symbol(
                            LocalId(primitive_type.0),
                        ));
                    }
                }
            }

            match result {
                SymbolResolutionResult::Resolved(symbol) => {
                    object.state = RelationState::Resolved(symbol);
                }
                SymbolResolutionResult::DeadImport => {
                    self.diagnostics
                        .push(diagnose_invalid_symbol_from_dead_import(
                            self.engine,
                            origin,
                            self.externals.current,
                            self.imports.get_imports(origin).unwrap(),
                            relation_id,
                            symbol_name,
                        ));
                    object.state = RelationState::Dead;
                }
                SymbolResolutionResult::Invalid(symbol) => {
                    let env_var = self
                        .engine
                        .get_environment(symbol.source)
                        .expect("resolved symbol points to an unknown environment");
                    let var = env_var
                        .symbols
                        .get(symbol.object_id)
                        .expect("resolved symbol points to an unknown variable in environment");
                    let mut occurrences: Vec<_> =
                        origin_env.find_references(SymbolRef::External(relation_id));
                    occurrences.sort_by_key(|s| s.start);

                    self.diagnostics.push(diagnose_invalid_symbol(
                        var.ty,
                        origin,
                        self.externals.current,
                        symbol_name,
                        &occurrences,
                    ));
                    object.state = RelationState::Dead;
                }
                // All attempts failed to resolve the symbol
                SymbolResolutionResult::NotFound => {
                    let symbol_env_name = symbol_name.tail().unwrap_or(symbol_name.clone());

                    if symbol_name.is_qualified() && !visited.contains(&symbol_env_name) {
                        // We put the unknown name in the visitable
                        to_visit.push(symbol_env_name);
                        // the name wasn't known from the analyzer, let's give it a chance to be resolved
                        continue;
                    } //if the name were already requested, it's definitely unresolvable

                    // If we reach this point, the symbol could not be resolved, during any of the previous phases / cycles.
                    self.diagnostics.push(diagnose_unresolved_external_symbols(
                        relation_id,
                        origin,
                        self.externals.current,
                        origin_env,
                        symbol_name,
                    ));
                    object.state = RelationState::Dead
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use indexmap::IndexMap;
    //use pretty_assertions::assert_eq;

    use context::source::Source;
    use context::str_find::{find_in, find_in_nth};
    use parser::parse_trusted;

    use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
    use crate::engine::Engine;
    use crate::environment::symbols::{SymbolLocation, SymbolRegistry};
    use crate::importer::StaticImporter;
    use crate::imports::{Imports, ResolvedImport, SourceImports, UnresolvedImport};
    use crate::name::Name;
    use crate::reef::{Externals, Reef, ReefId, LANG_REEF};
    use crate::relations::{
        LocalId, Relation, RelationId, RelationState, Relations, ResolvedSymbol, SourceId,
    };
    use crate::steps::collect::SymbolCollector;
    use crate::steps::resolve::SymbolResolver;
    use crate::steps::resolve_sources;
    use crate::types::INT;
    use crate::{resolve_all, ResolutionResult};

    #[test]
    fn test_reefs_external_symbols_resolution() {
        let mut reefs = Externals::default();

        fn define_reef<'e, const N: usize>(
            externals: &Externals,
            sources: [(Name, Source<'e>); N],
        ) -> (Vec<Diagnostic>, ResolutionResult<'e>) {
            let mut diagnostics = Vec::new();
            let mut result = ResolutionResult::default();

            resolve_sources(
                sources.iter().map(|(n, _)| n.clone()).collect(),
                &mut result,
                &mut StaticImporter::new(sources, parse_trusted),
                externals,
                &mut diagnostics,
            );

            (diagnostics, result)
        }

        let (diagnostics, result) = define_reef(
            &reefs,
            [(
                Name::new("std"),
                Source::unknown("fun foo() -> Exitcode = echo stdlib"),
            )],
        );
        assert_eq!(diagnostics, vec![]);
        reefs.register(Reef::new_partial(
            "std".to_owned(),
            result.engine,
            result.relations,
        ));

        let main_source = "use std::foo; use reef::std::foo as my_foo; reef::std::foo(); std::foo(); foo(); my_foo()";
        let (diagnostics, result) = define_reef(
            &reefs,
            [
                (Name::new("main"), Source::unknown(main_source)),
                (
                    Name::new("std"),
                    Source::unknown("fun foo() -> Exitcode = echo fake stdlib"),
                ),
            ],
        );
        assert_eq!(diagnostics, vec![]);
        reefs.register(Reef::new_partial(
            "test".to_owned(),
            result.engine,
            result.relations,
        ));

        let reef = reefs.get_reef(ReefId(2)).unwrap();
        assert_eq!(reef.name, "test");
        assert_eq!(reefs.get_reef(ReefId(1)).unwrap().name, "std");
        assert_eq!(reefs.get_reef(LANG_REEF).unwrap().name, "lang");

        assert_eq!(
            result.imports.get_imports(SourceId(2)).unwrap(),
            &SourceImports::with(
                IndexMap::new(),
                HashMap::from([
                    (
                        "foo".to_string(),
                        (
                            ResolvedImport::Symbols(HashMap::from([(
                                SymbolRegistry::Objects,
                                ResolvedSymbol::new(ReefId(1), SourceId(0), LocalId(0))
                            )])),
                            find_in(main_source, "std::foo")
                        )
                    ),
                    (
                        "my_foo".to_string(),
                        (
                            ResolvedImport::Symbols(HashMap::from([(
                                SymbolRegistry::Objects,
                                ResolvedSymbol::new(ReefId(2), SourceId(0), LocalId(0))
                            )])),
                            find_in(main_source, "reef::std::foo as my_foo")
                        )
                    ),
                ])
            )
        );

        assert_eq!(
            reef.relations.iter().collect::<Vec<_>>(),
            vec![
                (
                    // this one is foo's return type relation with Exitcode lang type
                    RelationId(0),
                    &Relation::resolved(
                        SourceId(1),
                        ResolvedSymbol::lang_symbol(LocalId(4)),
                        SymbolRegistry::Types,
                    )
                ),
                // main's imports relations
                (
                    RelationId(1),
                    &Relation::resolved(
                        SourceId(2),
                        ResolvedSymbol::new(ReefId(2), SourceId(0), LocalId(0)),
                        SymbolRegistry::Objects,
                    )
                ),
                (
                    RelationId(2),
                    &Relation::resolved(
                        SourceId(2),
                        ResolvedSymbol::new(ReefId(1), SourceId(0), LocalId(0)),
                        SymbolRegistry::Objects,
                    )
                ),
                (
                    RelationId(3),
                    &Relation::resolved(
                        SourceId(2),
                        ResolvedSymbol::new(ReefId(1), SourceId(0), LocalId(0)),
                        SymbolRegistry::Objects,
                    )
                ),
                (
                    RelationId(4),
                    &Relation::resolved(
                        SourceId(2),
                        ResolvedSymbol::new(ReefId(2), SourceId(0), LocalId(0)),
                        SymbolRegistry::Objects,
                    )
                ),
            ]
        )
    }

    #[test]
    fn report_unknown_imported_symbol() {
        let mut importer = StaticImporter::new(
            [
                (Name::new("main"), Source::new("reef::math::id(9)", "main")),
                (
                    Name::new("math"),
                    Source::new("fun id(n: Int) -> Int = $n\nfun dummy() = {}", "math"),
                ),
            ],
            parse_trusted,
        );
        let externals = Externals::default();
        let mut diagnostics = Vec::new();
        let res = resolve_all(
            Name::new("main"),
            &externals,
            &mut importer,
            &mut diagnostics,
        );
        assert_eq!(diagnostics, vec![]);
        assert_eq!(
            res.relations
                .iter()
                .map(|(_, r)| r.clone())
                .collect::<Vec<_>>(),
            vec![
                Relation::resolved(
                    SourceId(0),
                    ResolvedSymbol::new(ReefId(1), SourceId(1), LocalId(0)),
                    SymbolRegistry::Objects,
                ),
                Relation::resolved(
                    SourceId(2),
                    ResolvedSymbol::lang_symbol(LocalId(INT.type_id.0)),
                    SymbolRegistry::Types,
                ),
            ]
        )
    }

    #[test]
    fn test_capture() {
        let src = "\
        fun foo() = {
            var x = 1
            fun foo1() = {
                echo $x
                fun foo2() = {
                    echo $x
                }
            }
        }
        ";
        let mut importer = StaticImporter::new(
            [(Name::new("main"), Source::new(src, "main"))],
            parse_trusted,
        );
        let externals = Externals::default();
        let mut diagnostics = Vec::new();
        let res = resolve_all(
            Name::new("main"),
            &externals,
            &mut importer,
            &mut diagnostics,
        );
        assert_eq!(diagnostics, vec![]);
        assert_eq!(
            res.relations.iter().collect::<Vec<_>>(),
            vec![
                (
                    RelationId(0),
                    &Relation::resolved(
                        SourceId(2),
                        ResolvedSymbol::new(ReefId(1), SourceId(1), LocalId(0)),
                        SymbolRegistry::Objects,
                    )
                ),
                (
                    RelationId(1),
                    &Relation::resolved(
                        SourceId(3),
                        ResolvedSymbol::new(ReefId(1), SourceId(1), LocalId(0)),
                        SymbolRegistry::Objects,
                    )
                ),
            ]
        )
    }

    #[test]
    fn test_reef_imports_resolution() {
        let math_src = Source::unknown("val PI = 3.14");
        let std_src = Source::unknown("val Foo = 'moshell_std'; val Bar = $Foo");
        let io_src = Source::unknown("fun output() = (); fun input() = ()");
        let test_src = "
            use reef::math::PI
            use reef::std::{io, foo}
            use reef::std::*
            use reef::std::io::{input, output}
        ";

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();
        let mut importer = StaticImporter::new(
            [
                (Name::new("math"), math_src),
                (Name::new("std"), std_src),
                (Name::new("std::io"), io_src),
                (Name::new("test"), Source::unknown(test_src)),
            ],
            parse_trusted,
        );
        let mut diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);
        let test_env_imports = imports.get_imports_mut(SourceId(0)).unwrap();
        assert_eq!(
            test_env_imports,
            &SourceImports::with(
                IndexMap::from([
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            loc: SymbolLocation::in_current_reef(Name::new("math::PI")),
                        },
                        find_in(test_src, "reef::math::PI")
                    ),
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            loc: SymbolLocation::in_current_reef(Name::new("std::io")),
                        },
                        find_in(test_src, "io")
                    ),
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            loc: SymbolLocation::in_current_reef(Name::new("std::foo")),
                        },
                        find_in(test_src, "foo")
                    ),
                    (
                        UnresolvedImport::AllIn(SymbolLocation::in_current_reef(Name::new("std"))),
                        find_in(test_src, "reef::std::*")
                    ),
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            loc: SymbolLocation::in_current_reef(Name::new("std::io::input")),
                        },
                        find_in(test_src, "input")
                    ),
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            loc: SymbolLocation::in_current_reef(Name::new("std::io::output")),
                        },
                        find_in(test_src, "output")
                    ),
                ]),
                HashMap::new()
            ),
        );

        SymbolResolver::resolve_imports(
            &Externals::default(),
            &engine,
            SourceId(0),
            test_env_imports,
            &mut diagnostics,
        );
        assert_eq!(to_visit, vec![]);
        assert_eq!(
            diagnostics,
            vec![Diagnostic::new(
                DiagnosticID::ImportResolution,
                "unable to find imported symbol `foo` in module `std`.",
            )
            .with_observation((SourceId(0), ReefId(1), find_in(test_src, "foo")).into())]
        );

        assert_eq!(
            test_env_imports,
            &SourceImports::with(
                IndexMap::default(),
                HashMap::from([
                    (
                        "io".to_string(),
                        (ResolvedImport::Env(SourceId(1)), find_in(test_src, "io"))
                    ),
                    (
                        "foo".to_string(),
                        (ResolvedImport::Dead, find_in(test_src, "foo"))
                    ),
                    (
                        "PI".to_string(),
                        (
                            ResolvedImport::Symbols(HashMap::from([(
                                SymbolRegistry::Objects,
                                ResolvedSymbol::new(ReefId(1), SourceId(5), LocalId(0))
                            )])),
                            find_in(test_src, "reef::math::PI")
                        )
                    ),
                    (
                        "Bar".to_string(),
                        (
                            ResolvedImport::Symbols(HashMap::from([(
                                SymbolRegistry::Objects,
                                ResolvedSymbol::new(ReefId(1), SourceId(4), LocalId(1))
                            )])),
                            find_in(test_src, "reef::std::*")
                        )
                    ),
                    (
                        "Foo".to_string(),
                        (
                            ResolvedImport::Symbols(HashMap::from([(
                                SymbolRegistry::Objects,
                                ResolvedSymbol::new(ReefId(1), SourceId(4), LocalId(0))
                            )])),
                            find_in(test_src, "reef::std::*")
                        )
                    ),
                    (
                        "output".to_string(),
                        (
                            ResolvedImport::Symbols(HashMap::from([(
                                SymbolRegistry::Objects,
                                ResolvedSymbol::new(ReefId(1), SourceId(1), LocalId(0))
                            )])),
                            find_in(test_src, "output")
                        )
                    ),
                    (
                        "input".to_string(),
                        (
                            ResolvedImport::Symbols(HashMap::from([(
                                SymbolRegistry::Objects,
                                ResolvedSymbol::new(ReefId(1), SourceId(1), LocalId(1))
                            )])),
                            find_in(test_src, "input")
                        )
                    ),
                ])
            )
        );
    }

    #[test]
    fn test_symbols_resolution() {
        let math_src = Source::unknown("val PI = 3.14");

        let std_src = Source::unknown("val Foo = 'moshell_std'; val Bar = $Foo");

        let io_src = Source::unknown("val output = 'OutputStream()'; val input = 'InputStream()'");

        let test_src = Source::unknown(
            "\
            use reef::math::PI
            use reef::std::{Bar, io::*}

            fun foo() = $x

            val output = $output
            val x = $Bar
            val y = $PI
        ",
        );

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut importer = StaticImporter::new(
            [
                (Name::new("math"), math_src),
                (Name::new("std"), std_src),
                (Name::new("std::io"), io_src),
                (Name::new("test"), test_src),
            ],
            parse_trusted,
        );

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);
        let diagnostics = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
        );
        assert_eq!(diagnostics, vec![]);

        assert_eq!(
            relations.iter().map(|(_, r)| r.clone()).collect::<Vec<_>>(),
            vec![
                Relation::resolved(
                    SourceId(1),
                    ResolvedSymbol::new(ReefId(1), SourceId(0), LocalId(2)),
                    SymbolRegistry::Objects,
                ),
                Relation::resolved(
                    SourceId(0),
                    ResolvedSymbol::new(ReefId(1), SourceId(2), LocalId(0)),
                    SymbolRegistry::Objects,
                ),
                Relation::resolved(
                    SourceId(0),
                    ResolvedSymbol::new(ReefId(1), SourceId(3), LocalId(1)),
                    SymbolRegistry::Objects,
                ),
                Relation::resolved(
                    SourceId(0),
                    ResolvedSymbol::new(ReefId(1), SourceId(4), LocalId(0)),
                    SymbolRegistry::Objects,
                ),
            ]
        )
    }

    #[test]
    fn test_qualified_symbols_resolution() {
        let math_src = Source::unknown("fun add(a: Int, b: Int) = a + b");
        let math_advanced_src = Source::unknown("fun multiply(a: Int, b: Int) = a * b");
        let std_src = Source::unknown("fun foo() = 45; fun bar() = 78");
        let test_src = Source::unknown(
            "\
            use reef::math::advanced

            val x = reef::std::foo()
            val y = reef::std::bar()
            val sum = reef::math::add($x + $y, advanced::multiply(reef::std::foo(), reef::std::bar()))
        ",
        );

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut importer = StaticImporter::new(
            [
                (Name::new("math"), math_src),
                (Name::new("std"), std_src),
                (Name::new("math::advanced"), math_advanced_src),
                (Name::new("test"), test_src),
            ],
            parse_trusted,
        );

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();

        //first cycle
        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);
        let diagnostics = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
        );
        assert_eq!(diagnostics, vec![]);
        to_visit.sort();
        to_visit.dedup();
        assert_eq!(to_visit, vec![Name::new("math"), Name::new("std")]);
        let mut visited_vec = visited.iter().cloned().collect::<Vec<_>>();
        visited_vec.sort();
        assert_eq!(
            visited_vec,
            vec![Name::new("math::advanced"), Name::new("test")]
        );

        //second cycle
        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);
        let diagnostics = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
        );
        assert_eq!(diagnostics, vec![]);
        assert_eq!(to_visit, vec![]);

        let mut visited_vec = visited.iter().cloned().collect::<Vec<_>>();
        visited_vec.sort();
        assert_eq!(
            visited_vec,
            vec![
                Name::new("math"),
                Name::new("math::advanced"),
                Name::new("std"),
                Name::new("test"),
            ]
        );

        assert_eq!(
            relations.iter().map(|(_, r)| r.clone()).collect::<Vec<_>>(),
            vec![
                Relation::resolved(
                    SourceId(0),
                    ResolvedSymbol::new(ReefId(1), SourceId(3), LocalId(0)),
                    SymbolRegistry::Objects,
                ),
                Relation::resolved(
                    SourceId(0),
                    ResolvedSymbol::new(ReefId(1), SourceId(3), LocalId(1)),
                    SymbolRegistry::Objects,
                ),
                Relation::resolved(
                    SourceId(0),
                    ResolvedSymbol::new(ReefId(1), SourceId(6), LocalId(0)),
                    SymbolRegistry::Objects,
                ),
                Relation::resolved(
                    SourceId(0),
                    ResolvedSymbol::new(ReefId(1), SourceId(1), LocalId(0)),
                    SymbolRegistry::Objects,
                ),
                Relation::resolved(
                    SourceId(2),
                    ResolvedSymbol::lang_symbol(LocalId(INT.type_id.0)),
                    SymbolRegistry::Types,
                ),
                Relation::resolved(
                    SourceId(7),
                    ResolvedSymbol::lang_symbol(LocalId(INT.type_id.0)),
                    SymbolRegistry::Types,
                ),
            ]
        )
    }

    #[test]
    fn test_symbol_invalid_inner_symbol() {
        let test_src = "\
            fun foo() = 75
            foo::x(); foo::y::z(); foo::y::z()

            fun bar() = {
                foo::y::z()
                foo::y::z()
                foo::y::z()
                foz::x()

                fun foz() = {
                    a::foo::in_local()
                }

                var a = 78

                foz::x()
            }
        ";

        let mut importer = StaticImporter::new(
            [(Name::new("test"), Source::unknown(test_src))],
            parse_trusted,
        );
        let externals = Externals::default();
        let mut diagnostics = Vec::new();
        let result = resolve_all(
            Name::new("test"),
            &externals,
            &mut importer,
            &mut diagnostics,
        );
        let relations = result.relations;

        assert_eq!(
            diagnostics,
            vec![
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    "`foo` is a function which cannot export any inner symbols"
                )
                .with_observation((SourceId(0), ReefId(1), find_in(test_src, "foo::x()")).into())
                .with_help("`x` is an invalid symbol in function `foo`"),
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    "`foo` is a function which cannot export any inner symbols"
                )
                .with_observation((SourceId(0), ReefId(1), find_in(test_src, "foo::y::z()")).into())
                .with_help("`y::z` is an invalid symbol in function `foo`"),
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    "`foo` is a function which cannot export any inner symbols"
                )
                .with_observation(
                    (
                        SourceId(0),
                        ReefId(1),
                        find_in_nth(test_src, "foo::y::z()", 1)
                    )
                        .into()
                )
                .with_help("`y::z` is an invalid symbol in function `foo`"),
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    "`foz` is a function which cannot export any inner symbols"
                )
                .with_observation(
                    (SourceId(2), ReefId(1), find_in_nth(test_src, "foz::x()", 1)).into()
                )
                .with_help("`x` is an invalid symbol in function `foz`"),
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    "`foo` is a function which cannot export any inner symbols",
                )
                .with_observation(
                    (
                        SourceId(2),
                        ReefId(1),
                        find_in_nth(test_src, "foo::y::z()", 2)
                    )
                        .into()
                )
                .with_observation(
                    (
                        SourceId(2),
                        ReefId(1),
                        find_in_nth(test_src, "foo::y::z()", 3)
                    )
                        .into()
                )
                .with_observation(
                    (
                        SourceId(2),
                        ReefId(1),
                        find_in_nth(test_src, "foo::y::z()", 4)
                    )
                        .into()
                )
                .with_help("`y::z` is an invalid symbol in function `foo`"),
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    "Could not resolve symbol `a::foo::in_local`."
                )
                .with_observation(
                    (
                        SourceId(3),
                        ReefId(1),
                        find_in(test_src, "a::foo::in_local()")
                    )
                        .into()
                ),
            ]
        );

        for (_, relation) in relations.iter() {
            assert_eq!(relation.state, RelationState::Dead)
        }
    }

    #[test]
    fn test_unknown_symbols() {
        let a_src = Source::unknown("val C = 'A'");

        let source = "\
        use reef::A::B
        use reef::B::C
        use C::*

        $a; $a; $a
        $C; $B;
        ";
        let test_src = Source::unknown(source);
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut importer = StaticImporter::new(
            [(Name::new("test"), test_src), (Name::new("A"), a_src)],
            parse_trusted,
        );

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);

        let diagnostics = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
        );
        to_visit.dedup();
        assert_eq!(to_visit, vec![]);
        assert_eq!(
            diagnostics,
            vec![
                Diagnostic::new(
                    DiagnosticID::ImportResolution,
                    "unable to find imported symbol `B` in module `A`.",
                )
                    .with_observation((SourceId(0), ReefId(1), find_in(source, "reef::A::B")).into()),
                Diagnostic::new(
                    DiagnosticID::ImportResolution,
                    "unable to find imported symbol `B::C`."
                )
                    .with_observation((SourceId(0), ReefId(1), find_in(source, "reef::B::C")).into()),
                Diagnostic::new(
                    DiagnosticID::ImportResolution,
                    "unable to find reef `C`."
                )

                    .with_observation((SourceId(0), ReefId(1), find_in(source, "C::*")).into()),
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    "Could not resolve symbol `a`."
                )
                    .with_observation((SourceId(0), ReefId(1), find_in_nth(source, "$a", 0)).into())
                    .with_observation((SourceId(0), ReefId(1), find_in_nth(source, "$a", 1)).into())
                    .with_observation((SourceId(0), ReefId(1), find_in_nth(source, "$a", 2)).into()),
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    "unresolvable symbol `C` has no choice but to be ignored due to invalid import of `C`."
                )
                    .with_observation(Observation::context(SourceId(0), ReefId(1), find_in_nth(source, "reef::B::C", 0), "invalid import introduced here"))
                    .with_observation((SourceId(0), ReefId(1), find_in(source, "$C")).into()),
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    "unresolvable symbol `B` has no choice but to be ignored due to invalid import of `B`."
                )
                    .with_observation(Observation::context(SourceId(0), ReefId(1), find_in_nth(source, "reef::A::B", 0), "invalid import introduced here"))
                    .with_observation((SourceId(0), ReefId(1), find_in(source, "$B")).into()),
            ]
        )
    }

    #[test]
    fn test_global_unknown_symbols() {
        let source = "\
        $C; $C
        var C = 45
        $a; $a; $a
        $C; $C;
        ";
        let test_src = Source::unknown(source);
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut importer = StaticImporter::new([(Name::new("test"), test_src)], parse_trusted);

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);

        let diagnostic = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
        );

        assert_eq!(
            diagnostic,
            vec![
                Diagnostic::new(DiagnosticID::UnknownSymbol, "Could not resolve symbol `C`.")
                    .with_observation((SourceId(0), ReefId(1), find_in_nth(source, "$C", 0)).into())
                    .with_observation(
                        (SourceId(0), ReefId(1), find_in_nth(source, "$C", 1)).into()
                    ),
                Diagnostic::new(DiagnosticID::UnknownSymbol, "Could not resolve symbol `a`.")
                    .with_observation((SourceId(0), ReefId(1), find_in_nth(source, "$a", 0)).into())
                    .with_observation((SourceId(0), ReefId(1), find_in_nth(source, "$a", 1)).into())
                    .with_observation(
                        (SourceId(0), ReefId(1), find_in_nth(source, "$a", 2)).into()
                    ),
            ]
        )
    }

    #[test]
    fn test_local_unknown_symbols() {
        let source = "\
        fun foo() = {
            $C; $C
            var C = 45
            $a; $a; $a
            $C; $C;
        }
        ";
        let test_src = Source::unknown(source);
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut importer = StaticImporter::new([(Name::new("test"), test_src)], parse_trusted);

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);

        let diagnostic = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
        );

        assert_eq!(
            diagnostic,
            vec![
                Diagnostic::new(DiagnosticID::UnknownSymbol, "Could not resolve symbol `C`.",)
                    .with_observation((SourceId(1), ReefId(1), find_in(source, "$C")).into())
                    .with_observation(
                        (SourceId(1), ReefId(1), find_in_nth(source, "$C", 1)).into()
                    ),
                Diagnostic::new(DiagnosticID::UnknownSymbol, "Could not resolve symbol `a`.",)
                    .with_observation((SourceId(1), ReefId(1), find_in_nth(source, "$a", 0)).into())
                    .with_observation((SourceId(1), ReefId(1), find_in_nth(source, "$a", 1)).into())
                    .with_observation(
                        (SourceId(1), ReefId(1), find_in_nth(source, "$a", 2)).into()
                    ),
            ]
        )
    }

    #[test]
    fn find_in_parent_environment() {
        let source = Source::unknown("val found = 'false'; fun find() = $found");

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut imports = Imports::default();
        let mut importer = StaticImporter::new([(Name::new("test"), source)], parse_trusted);

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
            &mut importer,
        );

        assert_eq!(diagnostics, vec![]);

        let diagnostics = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
            &Externals::default(),
            &mut to_visit,
            &mut visited,
        );

        assert_eq!(diagnostics, vec![]);

        assert_eq!(
            relations.iter().map(|(_, r)| r.clone()).collect::<Vec<_>>(),
            vec![Relation::resolved(
                SourceId(1),
                ResolvedSymbol::new(ReefId(1), SourceId(0), LocalId(0)),
                SymbolRegistry::Objects,
            )]
        )
    }
}
