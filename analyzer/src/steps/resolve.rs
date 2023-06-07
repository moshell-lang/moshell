use std::collections::HashSet;
use std::iter::once;

use context::source::SourceSegment;

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::environment::variables::TypeInfo;
use crate::environment::Environment;
use crate::imports::{Imports, ResolvedImport, SourceImports, UnresolvedImport};
use crate::name::Name;
use crate::relations::{
    GlobalObjectId, ObjectId, ObjectState, Relations, ResolvedSymbol, SourceObjectId, Symbol,
};

/// The result of a symbol resolution attempt
#[derive(PartialEq)]
enum SymbolResolutionResult {
    /// The symbol is resolved, where `ResolvedSymbol` is the resolved symbol
    Resolved(ResolvedSymbol),
    /// The symbol is resolved, but it's invalid. This result usually implies a diagnostic emission.
    /// Where the `ResolvedSymbol` is the resolved symbol
    Invalid(ResolvedSymbol),
    /// The symbol is imported but its import was invalidated
    DeadImport,
    /// The symbol could not be found.
    NotFound,
}

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
                        relations.objects[object_id.0].state = ObjectState::Dead;
                    } else {
                        let symbol = ResolvedSymbol {
                            source: *env_id,
                            object_id: local,
                        };
                        relations.objects[object_id.0].state = ObjectState::Resolved(symbol);
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

    fn resolve_symbol_from_locals(
        env_id: SourceObjectId,
        env: &Environment,
        symbol_name: &Name,
    ) -> SymbolResolutionResult {
        if env.has_strict_declaration_order() {
            return SymbolResolutionResult::NotFound;
        }
        if let Some(var_id) = env.variables.get_exported(symbol_name.root()) {
            let symbol = ResolvedSymbol {
                source: env_id,
                object_id: var_id,
            };
            if symbol_name.is_qualified() {
                return SymbolResolutionResult::Invalid(symbol);
            }
            return SymbolResolutionResult::Resolved(symbol);
        }

        SymbolResolutionResult::NotFound
    }

    fn resolve_absolute_symbol(engine: &Engine, name: &Name) -> SymbolResolutionResult {
        // As we could not resolve the symbol using imports, try to find the symbol from
        // an absolute qualified name
        let env_name = name.tail().unwrap_or(name.clone());
        let env_result = get_mod_from_absolute(engine, &env_name);

        if let Some((env_id, env)) = env_result {
            let resolved_pos = env.variables.find_id(name.simple_name());

            if let Some(symbol_pos) = resolved_pos {
                let symbol = ResolvedSymbol::new(env_id, symbol_pos);
                return SymbolResolutionResult::Resolved(symbol);
            }
        }
        SymbolResolutionResult::NotFound
    }

    /// resolves the symbols of given environment, using given resolved imports for external symbol references.
    fn resolve_symbol_from_imports(
        engine: &Engine,
        imports: &SourceImports,
        name: &Name,
    ) -> SymbolResolutionResult {
        let name_root = name.root();

        // try to resolve the relation by looking the name's root inside imports
        match imports.get_import(name_root) {
            Some(ResolvedImport::Symbol(resolved_symbol)) => {
                return if !name.is_qualified() {
                    SymbolResolutionResult::Resolved(*resolved_symbol)
                } else {
                    SymbolResolutionResult::Invalid(*resolved_symbol)
                }
            }
            Some(ResolvedImport::Env(resolved_module)) => {
                let env = engine
                    .get_environment(*resolved_module)
                    .expect("resolved import points to an unknown environment");

                let resolved_pos = env
                    .variables
                    .exported_vars()
                    .position(|v| v.name == name.simple_name());

                if let Some(symbol_pos) = resolved_pos {
                    return SymbolResolutionResult::Resolved(ResolvedSymbol::new(
                        *resolved_module,
                        symbol_pos,
                    ));
                }
            }
            Some(ResolvedImport::Dead) => return SymbolResolutionResult::DeadImport,
            None => {} //simply fallback to NotFound
        };
        SymbolResolutionResult::NotFound
    }

    /// Attempts to resolve all given unresolved imports, returning a [ResolvedImports] structure containing the
    /// imports that could get resolved.
    /// This method will append a new diagnostic for each imports that could not be resolved.
    fn resolve_imports(
        env_id: SourceObjectId,
        imports: &mut SourceImports,
        engine: &Engine,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        //iterate over our unresolved imports
        for (unresolved, segment) in imports.take_unresolved_imports() {
            match unresolved {
                // If the unresolved import is a symbol
                UnresolvedImport::Symbol {
                    alias,
                    qualified_name: name,
                } => {
                    // try to get referenced module of the import
                    let result = get_mod_from_relatives(imports, &name, engine);
                    match result {
                        // if the environment wasn't found, attempt to resolve it in next cycle
                        None => {
                            // if the environment wasn't found, and its name was already known, push a diagnostic as it does not exists
                            let diagnostic =
                                diagnose_unresolved_import(env_id, &name, None, segment.clone());
                            diagnostics.push(diagnostic);
                            imports.set_resolved_import(
                                alias.unwrap_or(name.simple_name().to_string()),
                                ResolvedImport::Dead,
                                segment,
                            );
                        }
                        //else, try to resolve it
                        Some((found_env_id, found_env)) => {
                            let symbol_name = name.simple_name().to_string();
                            if found_env.fqn == name {
                                //it's the environment that is being imported
                                imports.set_resolved_import(
                                    alias.unwrap_or(symbol_name),
                                    ResolvedImport::Env(found_env_id),
                                    segment,
                                );
                                continue;
                            }

                            let symbol_id = found_env.variables.get_exported(&symbol_name);

                            if let Some(symbol_id) = symbol_id {
                                let resolved = ResolvedSymbol::new(found_env_id, symbol_id);
                                imports.set_resolved_import(
                                    alias.unwrap_or(symbol_name),
                                    ResolvedImport::Symbol(resolved),
                                    segment,
                                );
                                continue;
                            }
                            // the symbol inside the resolved environment could not be found
                            let diagnostic = diagnose_unresolved_import(
                                env_id,
                                &name,
                                Some(found_env.fqn.clone()),
                                segment.clone(),
                            );
                            imports.set_resolved_import(
                                alias.unwrap_or(symbol_name),
                                ResolvedImport::Dead,
                                segment,
                            );
                            diagnostics.push(diagnostic);
                        }
                    }
                }

                //if the unresolved import is an 'AllIn' import, meaning that it imports all symbols from given module
                UnresolvedImport::AllIn(name) => {
                    // try to get referenced environment of the import
                    match get_mod_from_relatives(imports, &name, engine) {
                        None => {
                            // if the environment wasn't found, and its name was already known, push a diagnostic as it does not exists
                            let diagnostic =
                                diagnose_unresolved_import(env_id, &name, None, segment.clone());
                            diagnostics.push(diagnostic);
                        }
                        Some((env_id, env)) => {
                            for var in env.variables.exported_vars() {
                                let var_id = env.variables.get_exported(&var.name).unwrap();

                                let import_symbol = ResolvedSymbol::new(env_id, var_id);
                                imports.set_resolved_import(
                                    var.name.clone(),
                                    ResolvedImport::Symbol(import_symbol),
                                    segment.clone(),
                                );
                            }
                        }
                    }
                }
            }
        }
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

fn get_mod_from_relatives<'a>(
    imports: &SourceImports,
    name: &Name,
    engine: &'a Engine,
) -> Option<(SourceObjectId, &'a Environment)> {
    let name_parts = name.parts();
    let (name_root, name_parts) = name_parts.split_first().unwrap(); //name_parts cannot be empty

    let target_env_id = match imports.get_import(name_root) {
        Some(ResolvedImport::Dead) => return None, //if the import is found but dead, assume it's simply not found
        Some(ResolvedImport::Symbol(symbol)) => symbol.source,
        Some(ResolvedImport::Env(module)) => *module,
        None => return get_mod_from_absolute(engine, name),
    };

    let base_env = engine
        .get_environment(target_env_id)
        .expect("import points to an unknown environment");
    let base_fqn = base_env.fqn.clone();

    let name_fq = (!name_parts.is_empty())
        .then(|| base_fqn.appended(Name::from(name_parts)))
        .unwrap_or(base_fqn);

    get_mod_from_absolute(engine, &name_fq)
}

/// Gets a module environment from the given fully qualified name, then pop the name until it finds a valid environment.
/// returns None if the name's root could not get resolved
fn get_mod_from_absolute<'a>(
    engine: &'a Engine,
    name: &Name,
) -> Option<(SourceObjectId, &'a Environment)> {
    let mut env_name = Some(name.clone());
    while let Some(name) = env_name {
        if let Some((id, env)) = engine.find_environment_by_name(&name) {
            if env.parent.is_none() {
                return Some((id, env));
            }
        }
        env_name = name.tail();
    }
    None
}

pub(crate) fn diagnose_invalid_symbol(
    base_type: TypeInfo,
    env_id: SourceObjectId,
    name: &Name,
    segments: &[SourceSegment],
) -> Diagnostic {
    let name_root = name.root();
    let (_, tail) = name.parts().split_first().unwrap();
    let base_type_name = base_type.to_string();
    let msg = format!("`{name_root}` is a {base_type_name} which cannot export any inner symbols");

    let mut observations: Vec<_> = segments
        .iter()
        .map(|seg| Observation::new(seg.clone()))
        .collect();
    observations.sort_by_key(|s| s.segment.start);

    Diagnostic::new(DiagnosticID::InvalidSymbol, env_id, msg)
        .with_observations(observations)
        .with_help(format!(
            "`{}` is an invalid symbol in {base_type_name} `{name_root}`",
            Name::from(tail)
        ))
}

/// Creates a diagnostic for a symbol being invalidated due to it's invalid import bound.
/// The caller must ensure that env_id is valid as well as the given name's root is contained in given env's variables.
pub(crate) fn diagnose_invalid_symbol_from_dead_import(
    engine: &Engine,
    env_id: SourceObjectId,
    env_imports: &SourceImports,
    global_relation: GlobalObjectId,
    name: &Name,
) -> Diagnostic {
    let name_root = name.root();

    let env = engine.get_environment(env_id).expect("invalid env id");
    let segments = env.find_references(global_relation.into());

    let msg = format!("unresolvable symbol `{name}` has no choice but to be ignored due to invalid import of `{name_root}`.");
    let invalid_import_seg = env_imports
        .get_import_segment(name_root)
        .expect("unknown import");

    let mut segments: Vec<_> = segments
        .iter()
        .map(|seg| Observation::new(seg.clone()))
        .collect();

    segments.sort_by_key(|s| s.segment.start);

    Diagnostic::new(DiagnosticID::InvalidSymbol, env_id, msg)
        .with_observation(Observation::with_help(
            invalid_import_seg,
            "invalid import introduced here",
        ))
        .with_observations(segments)
}

/// Appends a diagnostic for an external symbol that could not be resolved.
///
/// Each expression that use this symbol (such as variable references) will then get an observation.
fn diagnose_unresolved_external_symbols(
    relation: GlobalObjectId,
    env_id: SourceObjectId,
    env: &Environment,
    name: &Name,
) -> Diagnostic {
    let mut diagnostic = Diagnostic::new(
        DiagnosticID::UnknownSymbol,
        env_id,
        format!("Could not resolve symbol `{name}`."),
    );

    let mut observations: Vec<_> = env
        .list_definitions()
        .filter(|(_, sym)| match sym {
            Symbol::Local(_) => false,
            Symbol::Global(g) => *g == relation.0,
        })
        .map(|(seg, _)| Observation::new(seg.clone()))
        .collect();

    observations.sort_by_key(|s| s.segment.start);
    diagnostic.observations = observations;

    diagnostic
}

/// Appends a diagnostic for an import that could not be resolved.
/// Each `use` expressions that was referring to the unknown import will get a diagnostic
fn diagnose_unresolved_import(
    env_id: SourceObjectId,
    imported_symbol_name: &Name,
    known_parent: Option<Name>,
    dependent_segment: SourceSegment,
) -> Diagnostic {
    let msg = format!(
        "unable to find imported symbol `{}`{}.",
        known_parent
            .as_ref()
            .and_then(|p| imported_symbol_name.relative_to(p))
            .unwrap_or(imported_symbol_name.clone()),
        known_parent
            .map(|p| format!(" in module `{p}`"))
            .unwrap_or_default()
    );

    Diagnostic::new(DiagnosticID::ImportResolution, env_id, msg)
        .with_observation(Observation::new(dependent_segment))
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
    use crate::imports::{Imports, SourceImports, UnresolvedImport};
    use crate::name::Name;
    use crate::relations::{Object, ObjectState, Relations, ResolvedSymbol, SourceObjectId};
    use crate::resolve_all;
    use crate::steps::collect::SymbolCollector;
    use crate::steps::resolve::{ResolvedImport, SymbolResolver};

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
            use io::{input, output}
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
                            qualified_name: Name::new("io::input"),
                        },
                        find_in(test_src, "input")
                    ),
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            qualified_name: Name::new("io::output"),
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
                            ResolvedImport::Env(SourceObjectId(3)),
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
                            ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(6), 0)),
                            find_in(test_src, "math::PI")
                        )
                    ),
                    (
                        "Bar".to_string(),
                        (
                            ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(1), 1)),
                            find_in(test_src, "std::*")
                        )
                    ),
                    (
                        "Foo".to_string(),
                        (
                            ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(1), 0)),
                            find_in(test_src, "std::*")
                        )
                    ),
                    (
                        "output".to_string(),
                        (
                            ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(3), 0)),
                            find_in(test_src, "output")
                        )
                    ),
                    (
                        "input".to_string(),
                        (
                            ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(3), 1)),
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
            &mut engine,
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
