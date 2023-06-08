use std::collections::HashSet;
use std::iter::once;

use crate::diagnostic::Diagnostic;
use crate::engine::Engine;
use crate::environment::Environment;
use crate::imports::Imports;
use crate::name::Name;
use crate::relations::{ObjectId, ObjectState, Relations, ResolvedSymbol, SourceObjectId, Symbol};
use crate::steps::resolve::diagnostics::*;
use crate::steps::resolve::symbol::SymbolResolutionResult;
use crate::steps::shared_diagnostics::diagnose_invalid_symbol;

pub(in crate::steps::resolve) mod diagnostics;
pub(in crate::steps::resolve) mod import;
pub(in crate::steps::resolve) mod symbol;

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

    diagnostics: Vec<Diagnostic>,
}

impl<'a, 'e> SymbolResolver<'a, 'e> {
    ///Attempts to resolve the unresolved Engine's symbols contained in the given Relations.
    /// Returns a vector of diagnostics raised by the resolution process.
    pub fn resolve_symbols(
        engine: &'a Engine<'e>,
        relations: &'a mut Relations,
        imports: &'a mut Imports,
        to_visit: &mut Vec<Name>,
        visited: &mut HashSet<Name>,
    ) -> Vec<Diagnostic> {
        let mut resolver = Self::new(engine, relations, imports);
        resolver.resolve(to_visit, visited);
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
        relations: &mut Relations,
        capture_env: &Environment,
        capture_env_id: SourceObjectId,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        fn diagnose_invalid_symbol_in_capture(
            env_stack: Vec<&Environment>,
            capture_env_id: SourceObjectId,
            name: &Name,
            local: ObjectId,
            global: ObjectId,
        ) -> Diagnostic {
            let mut segments: Vec<_> = env_stack
                .iter()
                .flat_map(|env| env.find_references(Symbol::Global(global)))
                .collect();

            segments.sort_by_key(|s| s.start);

            //TODO support observations in foreign environments to include concerned symbol declaration in diagnostics
            let declaration_env = *env_stack.last().unwrap();

            let var = declaration_env.variables.get_var(local).unwrap();
            diagnose_invalid_symbol(var.ty, capture_env_id, name, &segments)
        }

        'capture: for (name, object_id) in capture_env.variables.external_vars() {
            for (pos, (env_id, env)) in env_stack.iter().rev().enumerate() {
                if let Some(local) = env.variables.get_reachable(name.root()) {
                    let object = &mut relations.objects[object_id.0];
                    if name.is_qualified() {
                        let erroneous_capture =
                            env_stack.iter().rev().take(pos + 1).map(|(_, s)| *s);
                        let erroneous_capture =
                            once(capture_env).chain(erroneous_capture).collect();

                        let diagnostic = diagnose_invalid_symbol_in_capture(
                            erroneous_capture,
                            capture_env_id,
                            name,
                            local,
                            object_id.0,
                        );
                        diagnostics.push(diagnostic);
                        object.state = ObjectState::Dead;
                    } else {
                        let symbol = ResolvedSymbol {
                            source: *env_id,
                            object_id: local,
                        };
                        object.state = ObjectState::Resolved(symbol);
                    }
                    continue 'capture;
                }
            }
        }
    }

    fn new(engine: &'a Engine<'e>, relations: &'a mut Relations, imports: &'a mut Imports) -> Self {
        Self {
            engine,
            relations,
            imports,
            diagnostics: Vec::new(),
        }
    }

    /// The starting point of the resolution phase.
    /// enables the resolution and pushes diagnostics if any symbol could not be resolved.
    fn resolve(&mut self, to_visit: &mut Vec<Name>, visited: &mut HashSet<Name>) {
        for (env_id, _) in self.engine.environments() {
            if let Some(imports) = self.imports.get_imports_mut(env_id) {
                Self::resolve_imports(env_id, imports, self.engine, &mut self.diagnostics);
            }
        }
        self.resolve_trees(to_visit, visited);
    }

    /// Iterates over remaining unresolved symbols
    /// This resolution supports parent lookups if the resolution could not
    fn resolve_trees(&mut self, to_visit: &mut Vec<Name>, visited: &mut HashSet<Name>) {
        //NOTE: the resolution does not insert new objects
        for (object_id, object) in self.relations.iter_mut() {
            if object.state != ObjectState::Unresolved {
                continue;
            }
            let origin = object.origin;

            // Get the local naming of the object
            let origin_env = self
                .engine
                .get_environment(origin)
                .expect("Environment declared an unknown parent");

            let symbol_name = origin_env
                .variables
                .get_external_symbol_name(object_id)
                .expect("Unknown object name");

            let mut result = SymbolResolutionResult::NotFound;

            // Go up the parent chain until we get a decisive result
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
                    result = Self::resolve_symbol_from_locals(env_id, env, symbol_name);
                }

                if result == SymbolResolutionResult::NotFound {
                    if let Some(imports) = &self.imports.get_imports(env_id) {
                        // If the symbol wasn't found from the environment locals, try to resolve using its imports
                        result =
                            Self::resolve_symbol_from_imports(self.engine, imports, symbol_name)
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
                result = Self::resolve_absolute_symbol(self.engine, symbol_name)
            }

            match result {
                SymbolResolutionResult::Resolved(symbol) => {
                    object.state = ObjectState::Resolved(symbol);
                }
                SymbolResolutionResult::DeadImport => {
                    self.diagnostics
                        .push(diagnose_invalid_symbol_from_dead_import(
                            self.engine,
                            origin,
                            self.imports.get_imports(origin).unwrap(),
                            object_id,
                            symbol_name,
                        ));
                    object.state = ObjectState::Dead;
                }
                SymbolResolutionResult::Invalid(symbol) => {
                    let env_var = self
                        .engine
                        .get_environment(symbol.source)
                        .expect("resolved symbol points to an unknown environment");
                    let var = env_var
                        .variables
                        .get_var(symbol.object_id)
                        .expect("resolved symbol points to an unknown variable in environment");
                    let mut occurrences: Vec<_> =
                        origin_env.find_references(Symbol::Global(object_id.0));
                    occurrences.sort_by_key(|s| s.start);

                    self.diagnostics.push(diagnose_invalid_symbol(
                        var.ty,
                        origin,
                        symbol_name,
                        &occurrences,
                    ));
                    object.state = ObjectState::Dead;
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
                        object_id,
                        origin,
                        origin_env,
                        symbol_name,
                    ));
                    object.state = ObjectState::Dead
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use indexmap::IndexMap;
    use pretty_assertions::assert_eq;

    use context::source::Source;
    use context::str_find::{find_in, find_in_nth};
    use parser::parse_trusted;

    use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
    use crate::engine::Engine;
    use crate::importer::StaticImporter;
    use crate::imports::{Imports, ResolvedImport, SourceImports, UnresolvedImport};
    use crate::name::Name;
    use crate::relations::{Object, ObjectState, Relations, ResolvedSymbol, SourceObjectId};
    use crate::resolve_all;
    use crate::steps::collect::SymbolCollector;
    use crate::steps::resolve::SymbolResolver;

    #[test]
    fn report_unknown_imported_symbol() {
        let mut importer = StaticImporter::new(
            [
                (
                    Name::new("main"),
                    //Source::new("use math::dummy\nmath::id(9)", "main"),
                    Source::new("math::id(9)", "main"),
                ),
                (
                    Name::new("math"),
                    Source::new("fun id(n: Int) -> Int = $n\nfun dummy() = {}", "math"),
                ),
            ],
            parse_trusted,
        );
        let res = resolve_all(Name::new("main"), &mut importer);
        assert_eq!(res.diagnostics, vec![]);
        assert_eq!(
            res.relations.objects,
            vec![Object::resolved(
                SourceObjectId(0),
                ResolvedSymbol::new(SourceObjectId(1), 0),
            )]
        )
    }

    #[test]
    fn test_imports_resolution() {
        let math_src = Source::unknown("val PI = 3.14");
        let std_src = Source::unknown("val Foo = 'moshell_std'; val Bar = $Foo");
        let io_src = Source::unknown("fun output() = (); fun input() = ()");
        let test_src = "
            use math::PI
            use std::{io, foo}
            use std::*
            use std::io::{input, output}
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
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        let test_env_imports = imports.get_imports_mut(SourceObjectId(0)).unwrap();
        assert_eq!(diagnostics, vec![]);
        assert_eq!(
            test_env_imports,
            &SourceImports::with(
                IndexMap::from([
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            qualified_name: Name::new("math::PI"),
                        },
                        find_in(test_src, "math::PI")
                    ),
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            qualified_name: Name::new("std::io"),
                        },
                        find_in(test_src, "io")
                    ),
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            qualified_name: Name::new("std::foo"),
                        },
                        find_in(test_src, "foo")
                    ),
                    (
                        UnresolvedImport::AllIn(Name::new("std")),
                        find_in(test_src, "std::*")
                    ),
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            qualified_name: Name::new("std::io::input"),
                        },
                        find_in(test_src, "input")
                    ),
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            qualified_name: Name::new("std::io::output"),
                        },
                        find_in(test_src, "output")
                    ),
                ]),
                HashMap::new()
            ),
        );

        SymbolResolver::resolve_imports(
            SourceObjectId(0),
            test_env_imports,
            &engine,
            &mut diagnostics,
        );
        assert_eq!(to_visit, vec![]);
        assert_eq!(
            diagnostics,
            vec![Diagnostic::new(
                DiagnosticID::ImportResolution,
                SourceObjectId(0),
                "unable to find imported symbol `foo` in module `std`.",
            )
            .with_observation(Observation::new(find_in(test_src, "foo")))]
        );

        assert_eq!(
            test_env_imports,
            &SourceImports::with(
                IndexMap::default(),
                HashMap::from([
                    (
                        "io".to_string(),
                        (
                            ResolvedImport::Env(SourceObjectId(1)),
                            find_in(test_src, "io")
                        )
                    ),
                    (
                        "foo".to_string(),
                        (ResolvedImport::Dead, find_in(test_src, "foo"))
                    ),
                    (
                        "PI".to_string(),
                        (
                            ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(12), 0)),
                            find_in(test_src, "math::PI")
                        )
                    ),
                    (
                        "Bar".to_string(),
                        (
                            ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(7), 1)),
                            find_in(test_src, "std::*")
                        )
                    ),
                    (
                        "Foo".to_string(),
                        (
                            ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(7), 0)),
                            find_in(test_src, "std::*")
                        )
                    ),
                    (
                        "output".to_string(),
                        (
                            ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(1), 0)),
                            find_in(test_src, "output")
                        )
                    ),
                    (
                        "input".to_string(),
                        (
                            ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(1), 1)),
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
            use math::PI
            use std::{Bar, io::*}

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
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);
        let diagnostics = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
            &mut to_visit,
            &mut visited,
        );
        assert_eq!(diagnostics, vec![]);

        assert_eq!(
            relations.objects,
            vec![
                Object::resolved(SourceObjectId(1), ResolvedSymbol::new(SourceObjectId(0), 2)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(2), 0)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(3), 1)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(4), 0)),
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
            use math::advanced

            val x = std::foo()
            val y = std::bar()
            val sum = math::add($x + $y, advanced::multiply(std::foo(), std::bar()))
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
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);
        let diagnostics = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
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
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);
        let diagnostics = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
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
            relations.objects,
            vec![
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(3), 0)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(3), 1)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(6), 0)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(1), 0)),
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

        let result = resolve_all(Name::new("test"), &mut importer);
        let diagnostics = result.diagnostics;
        let relations = result.relations;

        assert_eq!(
            diagnostics,
            vec![
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    SourceObjectId(0),
                    "`foo` is a function which cannot export any inner symbols"
                )
                .with_observation(Observation::new(find_in(test_src, "foo::x()")))
                .with_help("`x` is an invalid symbol in function `foo`"),
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    SourceObjectId(0),
                    "`foo` is a function which cannot export any inner symbols"
                )
                .with_observation(Observation::new(find_in(test_src, "foo::y::z()")))
                .with_help("`y::z` is an invalid symbol in function `foo`"),
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    SourceObjectId(0),
                    "`foo` is a function which cannot export any inner symbols"
                )
                .with_observation(Observation::new(find_in_nth(test_src, "foo::y::z()", 1)))
                .with_help("`y::z` is an invalid symbol in function `foo`"),
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    SourceObjectId(2),
                    "`foz` is a function which cannot export any inner symbols"
                )
                .with_observation(Observation::new(find_in_nth(test_src, "foz::x()", 1)))
                .with_help("`x` is an invalid symbol in function `foz`"),
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    SourceObjectId(2),
                    "`foo` is a function which cannot export any inner symbols",
                )
                .with_observation(Observation::new(find_in_nth(test_src, "foo::y::z()", 2)))
                .with_observation(Observation::new(find_in_nth(test_src, "foo::y::z()", 3)))
                .with_observation(Observation::new(find_in_nth(test_src, "foo::y::z()", 4)))
                .with_help("`y::z` is an invalid symbol in function `foo`"),
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    SourceObjectId(3),
                    "Could not resolve symbol `a::foo::in_local`."
                )
                .with_observation(Observation::new(find_in(test_src, "a::foo::in_local()"))),
            ]
        );

        for relation in relations.objects {
            assert_eq!(relation.state, ObjectState::Dead)
        }
    }

    #[test]
    fn test_unknown_symbols() {
        let a_src = Source::unknown("val C = 'A'");

        let source = "\
        use A::B
        use B::C
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
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);

        let diagnostics = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
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
                    SourceObjectId(0),
                    "unable to find imported symbol `B` in module `A`.",
                )
                .with_observation(Observation::new(find_in(source, "A::B"))),
                Diagnostic::new(
                    DiagnosticID::ImportResolution,
                    SourceObjectId(0),
                    "unable to find imported symbol `B::C`."
                )
                .with_observation(Observation::new(find_in(source, "B::C"))),
                Diagnostic::new(
                    DiagnosticID::ImportResolution,
                    SourceObjectId(0),
                    "unable to find imported symbol `C`."
                )
                .with_observation(Observation::new(find_in(source, "C::*"))),
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    SourceObjectId(0),
                    "Could not resolve symbol `a`."
                )
                .with_observation(Observation::new(find_in_nth(source, "$a", 0)))
                .with_observation(Observation::new(find_in_nth(source, "$a", 1)))
                .with_observation(Observation::new(find_in_nth(source, "$a", 2))),
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    SourceObjectId(0),
                    "unresolvable symbol `C` has no choice but to be ignored due to invalid import of `C`."
                )
                .with_observation(Observation::with_help(find_in_nth(source, "B::C", 0), "invalid import introduced here"))
                    .with_observation(Observation::new(find_in(source, "$C"))),
                Diagnostic::new(
                    DiagnosticID::InvalidSymbol,
                    SourceObjectId(0),
                    "unresolvable symbol `B` has no choice but to be ignored due to invalid import of `B`."
                )
                    .with_observation(Observation::with_help(find_in_nth(source, "A::B", 0), "invalid import introduced here"))
                    .with_observation(Observation::new(find_in(source, "$B"))),
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
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);

        let diagnostic = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
            &mut to_visit,
            &mut visited,
        );

        assert_eq!(
            diagnostic,
            vec![
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    SourceObjectId(0),
                    "Could not resolve symbol `C`."
                )
                .with_observation(Observation::new(find_in_nth(source, "$C", 0)))
                .with_observation(Observation::new(find_in_nth(source, "$C", 1))),
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    SourceObjectId(0),
                    "Could not resolve symbol `a`."
                )
                .with_observation(Observation::new(find_in_nth(source, "$a", 0)))
                .with_observation(Observation::new(find_in_nth(source, "$a", 1)))
                .with_observation(Observation::new(find_in_nth(source, "$a", 2))),
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
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);

        let diagnostic = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
            &mut to_visit,
            &mut visited,
        );

        assert_eq!(
            diagnostic,
            vec![
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    SourceObjectId(1),
                    "Could not resolve symbol `C`.",
                )
                .with_observation(Observation::new(find_in(source, "$C")))
                .with_observation(Observation::new(find_in_nth(source, "$C", 1))),
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    SourceObjectId(1),
                    "Could not resolve symbol `a`.",
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
        let mut imports = Imports::default();
        let mut importer = StaticImporter::new([(Name::new("test"), source)], parse_trusted);

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut imports,
            &mut to_visit,
            &mut visited,
            &mut importer,
        );

        assert_eq!(diagnostics, vec![]);

        let diagnostics = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut imports,
            &mut to_visit,
            &mut visited,
        );

        assert_eq!(diagnostics, vec![]);

        assert_eq!(
            relations.objects,
            vec![Object::resolved(
                SourceObjectId(1),
                ResolvedSymbol::new(SourceObjectId(0), 0)
            )]
        )
    }
}
