use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};

use context::source::SourceSegment;

use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::engine::Engine;
use crate::environment::Environment;
use crate::environment::variables::{TypeInfo, TypeUsage};
use crate::name::Name;
use crate::relations::{
    GlobalObjectId, Relations, ResolvedSymbol, SourceObjectId, Symbol, UnresolvedImport,
    UnresolvedImports,
};

#[derive(PartialEq, Eq, Debug)]
enum ResolvedImport {
    Symbol(ResolvedSymbol),
    Module(SourceObjectId),
}

/// Used by the resolve step to store resolved imports of an environment.
#[derive(Default, PartialEq)]
struct ResolvedImports {
    imported_symbols: HashMap<String, ResolvedImport>,
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
    fn set_import(&mut self, symbol_name: String, import: ResolvedImport) {
        self.imported_symbols.insert(symbol_name, import);
    }

    fn with(symbols: HashMap<String, ResolvedImport>) -> Self {
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
        to_visit: &mut Vec<Name>,
        visited: &mut HashSet<Name>,
    ) -> Vec<Diagnostic> {
        let mut resolver = Self::new(engine, relations);
        resolver.resolve(to_visit, visited);
        resolver.diagnostics
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
    fn resolve(&mut self, to_visit: &mut Vec<Name>, visited: &mut HashSet<Name>) {
        let mut unresolved_imports = self.relations.take_imports();

        for (env_id, env) in self.engine.environments() {
            let unresolved_imports = unresolved_imports.remove(&env_id).unwrap_or_default();

            let resolved_imports =
                self.resolve_imports(env_id, unresolved_imports, to_visit, visited);
            self.resolve_symbols_of(env_id, env, resolved_imports, to_visit, visited);
        }
    }

    /// resolves the symbols of given environment, using given resolved imports for external symbol references.
    fn resolve_symbols_of(
        &mut self,
        env_id: SourceObjectId,
        env: &Environment,
        imports: ResolvedImports,
        to_visit: &mut Vec<Name>,
        visited: &mut HashSet<Name>,
    ) {
        for (usage, relation) in env.variables.external_usages() {
            // If the object is already resolved, ignore it
            if self.relations.objects[relation.0].resolved.is_some() {
                continue;
            }
            let name = usage.name();
            let name_root = name.root();

            // try to resolve the relation by looking the first name's
            match imports.imported_symbols.get(name_root) {
                // symbol is resolved
                Some(ResolvedImport::Symbol(resolved_symbol)) => {
                    if name.parts().len() != 1 {
                        let targeted_env = self.engine.get_environment(resolved_symbol.source).expect("resolved symbol points to an unknown environment");
                        let targeted_var = targeted_env.variables.get_var(resolved_symbol.object_id).expect("resolved symbol points to an unknown variable in environment");
                        self.diagnostics.push(Self::diagnose_invalid_symbols(targeted_var.ty, env_id, env, usage));
                        continue
                    }
                    let object = &mut self.relations.objects[relation.0];
                    object.resolved = Some(*resolved_symbol);
                    continue;
                }
                Some(ResolvedImport::Module(resolved_module)) => {
                    let env = self.engine.get_environment(*resolved_module).expect("resolved import points to an unknown module");
                    let resolved_pos = env
                        .variables
                        .exported_vars()
                        .position(|v| v.name == name.simple_name());

                    if let Some(symbol_pos) = resolved_pos {
                        let object = &mut self.relations.objects[relation.0];
                        object.resolved =
                            Some(ResolvedSymbol::new(*resolved_module, symbol_pos));
                        continue;
                    }
                }
                None => {
                    // As we could not resolve the symbol using imports, try to find the symbol from
                    // an absolute qualified name
                    let env_name = name.tail().unwrap_or(name.clone());
                    let env_result = get_env_from_absolute(self.engine, &env_name);

                    if let Some((env_id, env)) = env_result {
                        let resolved_pos = env
                            .variables
                            .exported_vars()
                            .position(|v| v.name == name.simple_name());

                        if let Some(symbol_pos) = resolved_pos {
                            let object = &mut self.relations.objects[relation.0];
                            object.resolved = Some(ResolvedSymbol::new(env_id, symbol_pos));
                            continue;
                        }
                    }
                    // if the name isn't qualified, directly raise a diagnostic
                    if name.parts().len() != 1 && !visited.contains(&env_name) {
                        // We put the unknown name in the visitable
                        to_visit.push(env_name);
                        // the name wasn't known from the analyzer, let's give it a chance to be resolved
                        continue;
                    } //if the name were already requested, it's definitely unresolvable
                }
            };

            self.diagnose_unresolved_external_symbols(relation, env_id, env, name)
        }
    }

    fn diagnose_invalid_symbols(
        target_type: TypeInfo,
        env_id: SourceObjectId,
        env: &Environment,
        usage: &TypeUsage,
    ) -> Diagnostic {
        let name = usage.name();
        let name_root = name.root();
        let (_, tail) = name.parts().split_first().unwrap();
        let target_type_name = match target_type {
            TypeInfo::Variable => "variable",
            TypeInfo::Function => "function"
        };
        let msg = format!("{name_root} is a {target_type_name} which cannot export any inner symbols");
        let mut diagnostic = Diagnostic::new(DiagnosticID::InvalidSymbol, env_id, msg);

        let observations = env
            .list_definitions()
            .filter(|(_, def)| def.usage.as_ref() == Some(&usage))
            .map(|(seg, _)| Observation::with_help(seg.clone(), format!("{} is an invalid reference in {target_type_name} {name_root}", Name::from(tail))));
        diagnostic.observations = observations.collect();
        diagnostic
    }

    /// Appends a diagnostic for an external symbol that could not be resolved.
    /// Each expression that used this symbol (such as VarReferences) will then get an observation
    fn diagnose_unresolved_external_symbols(
        &mut self,
        relation: GlobalObjectId,
        env_id: SourceObjectId,
        env: &Environment,
        name: &Name,
    ) {
        let mut diagnostic = Diagnostic::new(
            DiagnosticID::UnknownSymbol,
            env_id,
            format!("Could not resolve symbol {name}."),
        );

        let observations = env
            .list_definitions()
            .filter(|(_, sym)| match sym.symbol {
                Symbol::Local(_) => false,
                Symbol::Global(g) => g == relation.0,
            })
            .map(|(seg, _)| Observation::new(seg.clone()));

        diagnostic.observations = observations.collect();

        self.diagnostics.push(diagnostic);
    }

    /// Appends a diagnostic for an import that could not be resolved.
    /// Each `use` expressions that was referring to the unknown import will get a diagnostic
    fn diagnose_unresolved_import(
        &mut self,
        env_id: SourceObjectId,
        imported_symbol_name: Name,
        known_parent: Option<Name>,
        dependent_segment: SourceSegment,
    ) {
        let msg = &format!(
            "unable to find imported symbol {}{}.",
            known_parent
                .as_ref()
                .and_then(|p| imported_symbol_name.relative_to(p))
                .unwrap_or(imported_symbol_name),
            known_parent
                .map(|p| format!(" in module {p}"))
                .unwrap_or_default()
        );

        let diagnostic = Diagnostic::new(DiagnosticID::ImportResolution, env_id, msg)
            .with_observation(Observation::new(dependent_segment));
        self.diagnostics.push(diagnostic)
    }

    /// Attempts to resolve all given unresolved imports, returning a [ResolvedImports] structure containing the
    /// imports that could get resolved.
    /// This method will append a new diagnostic for each imports that could not be resolved.
    fn resolve_imports(
        &mut self,
        env_id: SourceObjectId,
        imports: UnresolvedImports,
        to_visit: &mut Vec<Name>,
        visited: &mut HashSet<Name>,
    ) -> ResolvedImports {

        let mut resolved_imports = ResolvedImports::default();
        //iterate over our unresolved imports
        for (unresolved, dependent) in imports.imports {
            match unresolved {
                // If the unresolved import is a symbol
                UnresolvedImport::Symbol { alias, fqn: name } => {
                    // try to get referenced environment of the import
                    let result = get_env_from_relatives(&resolved_imports, &name, self.engine);
                    match result {
                        // if the environment wasn't found, attempt to resolve it in next cycle
                        None => {
                            if !visited.contains(&name) {
                                to_visit.push(name);
                                continue;
                            }
                            // if the environment wasn't found, and its name was already known, push a diagnostic as it does not exists
                            self.diagnose_unresolved_import(env_id, name, None, dependent)
                        }
                        //else, try to resolve it
                        Some((found_env_id, found_env)) => {
                            let symbol_name = name.simple_name().to_string();
                            if found_env.fqn == name {
                                //it's the module that is being imported
                                resolved_imports.set_import(
                                    alias.unwrap_or(symbol_name),
                                    ResolvedImport::Module(found_env_id),
                                );
                                continue;
                            }
                            let symbol_id = found_env
                                .variables
                                .exported_vars()
                                .position(|var| var.name == symbol_name);

                            if let Some(symbol_id) = symbol_id {
                                let resolved = ResolvedSymbol::new(found_env_id, symbol_id);
                                resolved_imports.set_import(
                                    alias.unwrap_or(symbol_name),
                                    ResolvedImport::Symbol(resolved),
                                );
                                continue;
                            }
                            //if the symbol inside the resolved environment could not be found,
                            self.diagnose_unresolved_import(
                                env_id,
                                name,
                                Some(found_env.fqn.clone()),
                                dependent,
                            )
                        }
                    }
                }

                //if the unreseloved import is an 'AllIn' import, meaning that it imports all symbols from given module
                UnresolvedImport::AllIn(name) => {
                    // try to get referenced environment of the import
                    match get_env_from_relatives(&resolved_imports, &name, self.engine) {
                        None => {
                            if !visited.contains(&name) {
                                to_visit.push(name);
                                continue;
                            }
                            // if the environment wasn't found, and its name was already known, push a diagnostic as it does not exists
                            self.diagnose_unresolved_import(env_id, name, None, dependent)
                        }
                        Some((env_id, env)) => {
                            for var in env.variables.exported_vars() {
                                let var_id = env
                                    .variables
                                    .exported_vars()
                                    .position(|v| v.name == var.name)
                                    .unwrap();

                                let import_symbol = ResolvedSymbol::new(env_id, var_id);
                                resolved_imports.set_import(
                                    var.name.clone(),
                                    ResolvedImport::Symbol(import_symbol),
                                );
                            }
                        }
                    }
                }
            }
        }
        resolved_imports
    }

    fn resolve_trees(&mut self) -> Result<(), String> {
        for (object_id, object) in self.relations.iter_mut() {
            if object.resolved.is_some() {
                continue;
            }
            let env = self
                .engine
                .get_environment(object.origin)
                .expect("Environment declared an unknown parent");
            let name = env
                .variables
                .get_external_symbol_usage(object_id)
                .expect("Unknown object name");

            let mut current = env;
            while let Some((module, env)) = current
                .parent
                .and_then(|id| self.engine.get_environment(id).map(|env| (id, env)))
            {
                if let Some(resolved) = env.variables.get_symbol(name) {
                    object.resolved = Some(match resolved {
                        Symbol::Local(local) => ResolvedSymbol {
                            source: module,
                            object_id: local,
                        },
                        Symbol::Global(_) => todo!("resolver.get_resolved(GlobalObjectId(global)).expect(\"Unknown global object\")")
                    });
                    break;
                }
                current = env;
            }
        }
        Ok(())
    }
}

fn get_env_from_relatives<'a>(imports: &ResolvedImports, name: &Name, engine: &'a Engine) -> Option<(SourceObjectId, &'a Environment)> {
    let name_parts = name.parts().to_vec();
    let (name_root, name_parts) = name_parts.split_first().unwrap(); //name_parts cannot be empty
    let target_env_id = match imports.imported_symbols.get(name_root) {
        Some(ResolvedImport::Symbol(symbol)) => {
            symbol.source
        }
        Some(ResolvedImport::Module(module)) => {
            *module
        }
        None => return get_env_from_absolute(engine, name)
    };

    let base_env = engine.get_environment(target_env_id).expect("import points to an unknown environment");
    let base_fqn = base_env.fqn.clone();

    let name_fq = (!name_parts.is_empty()).then(|| base_fqn.appended(Name::from(name_parts))).unwrap_or(base_fqn);
    get_env_from_absolute(engine,  &name_fq)
}

/// Gets an environment from the given fully qualified name, then pop the name until it finds a valid environment.
/// returns None if the name's root could not get resolved
fn get_env_from_absolute<'a>(engine: &'a Engine, name: &Name) -> Option<(SourceObjectId, &'a Environment)> {
    let mut env_name = Some(name.clone());
    while let Some(name) = env_name {
        if let Some((id, env)) = engine.find_environment_by_name(&name, false) {
            return Some((id, env));
        }
        env_name = name.tail();
    }
    None
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
    use crate::name::Name;
    use crate::relations::{
        Object, Relations, ResolvedSymbol, SourceObjectId, UnresolvedImport, UnresolvedImports,
    };
    use crate::steps::collect::SymbolCollector;
    use crate::steps::resolve::{ResolvedImport, ResolvedImports, SymbolResolver};

    #[test]
    fn test_imports_resolution() {
        let math_src = Source::unknown("val PI = 3.14");
        let std_src = Source::unknown("val Foo = 'moshell_std'; val Bar = $Foo");
        let io_src = Source::unknown("val output = 'OutputStream()'; val input = 'InputStream()'");
        let test_src =
            "
            use math::PI
            use std::{Bar, io}
            use io::*
        ";

        let mut engine = Engine::default();
        let mut relations = Relations::default();

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
        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut to_visit,
            &mut visited,
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
                        find_in(test_src, "math::PI")
                    ),
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            fqn: Name::new("std::Bar"),
                        },
                        find_in(test_src, "Bar")
                    ),
                    (
                        UnresolvedImport::Symbol {
                            alias: None,
                            fqn: Name::new("std::io"),
                        },
                        find_in(test_src, "io")
                    ),
                    (UnresolvedImport::AllIn(Name::new("io")), find_in(test_src, "io::*")),
                ]))
            )])
        );

        let unresolved_imports = relations.take_imports().remove(&SourceObjectId(0)).unwrap();
        let mut resolver = SymbolResolver::new(&engine, &mut relations);
        let resolved_imports = resolver.resolve_imports(
            SourceObjectId(0),
            unresolved_imports,
            &mut to_visit,
            &mut visited,
        );
        assert_eq!(to_visit, vec![]);
        assert_eq!(resolver.diagnostics, vec![]);
        assert_eq!(
            resolved_imports,
            ResolvedImports::with(HashMap::from([
                (
                    "PI".to_string(),
                    ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(3), 0))
                ),
                (
                    "Bar".to_string(),
                    ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(2), 1))
                ),
                (
                    "io".to_string(),
                    ResolvedImport::Module(SourceObjectId(1))
                ),
                (
                    "output".to_string(),
                    ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(1), 0))
                ),
                (
                    "input".to_string(),
                    ResolvedImport::Symbol(ResolvedSymbol::new(SourceObjectId(1), 1))
                ),
            ]))
        );
        assert_eq!(resolver.diagnostics, vec![])
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

            val output = $output
            val x = $Bar
            val y = $PI
        ",
        );

        let mut engine = Engine::default();
        let mut relations = Relations::default();
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
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);
        let diagnostics =
            SymbolResolver::resolve_symbols(&engine, &mut relations, &mut to_visit, &mut visited);
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
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);
        let diagnostics =
            SymbolResolver::resolve_symbols(&engine, &mut relations, &mut to_visit, &mut visited);
        assert_eq!(diagnostics, vec![]);
        to_visit.dedup();
        assert_eq!(to_visit, vec![Name::new("std"), Name::new("math")]);
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
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);
        let diagnostics =
            SymbolResolver::resolve_symbols(&engine, &mut relations, &mut to_visit, &mut visited);
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
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(5), 0)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(5), 1)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(3), 0)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(1), 0)),
            ]
        )
    }


    #[test]
    #[ignore]
    fn test_symbol_wrong_usage() {
        let test_src = "\
            val x = 78
            fun foo() = 75
            x(); $foo; $foo()
        ";

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut importer = StaticImporter::new(
            [
                (Name::new("test"), Source::unknown(test_src)),
            ],
            parse_trusted,
        );

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut to_visit,
            &mut visited,
            &mut importer,
        );

        assert_eq!(diagnostics, vec![]);
        let diagnostics = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut to_visit,
            &mut visited,
        );
        assert_eq!(diagnostics, vec![]);

        assert_eq!(
            relations.objects,
            vec![
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(5), 0)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(5), 1)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(3), 0)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(1), 0)),
            ]
        )
    }


    #[test]
    #[ignore]
    fn test_symbol_invalid_inner_symbol() {
        let test_src = "\
            fun foo() = 75
            foo::x(); foo::y::z()
        ";

        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut importer = StaticImporter::new(
            [
                (Name::new("test"), Source::unknown(test_src)),
            ],
            parse_trusted,
        );

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut to_visit,
            &mut visited,
            &mut importer,
        );

        assert_eq!(diagnostics, vec![]);
        let diagnostics = SymbolResolver::resolve_symbols(
            &engine,
            &mut relations,
            &mut to_visit,
            &mut visited,
        );
        assert_eq!(to_visit, vec![]);
        assert_eq!(diagnostics, vec![]);

        assert_eq!(
            relations.objects,
            vec![
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(5), 0)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(5), 1)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(3), 0)),
                Object::resolved(SourceObjectId(0), ResolvedSymbol::new(SourceObjectId(1), 0)),
            ]
        )
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
        let mut importer = StaticImporter::new(
            [(Name::new("test"), test_src), (Name::new("A"), a_src)],
            parse_trusted,
        );

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);

        let diagnostics =
            SymbolResolver::resolve_symbols(&engine, &mut relations, &mut to_visit, &mut visited);
        to_visit.dedup();
        assert_eq!(to_visit, vec![]);
        assert_eq!(
            diagnostics,
            vec![
                Diagnostic::new(
                    DiagnosticID::ImportResolution,
                    SourceObjectId(0),
                    "unable to find imported symbol B in module A.",
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
        let test_src = Source::unknown(source);
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut importer = StaticImporter::new([(Name::new("test"), test_src)], parse_trusted);

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut to_visit,
            &mut visited,
            &mut importer,
        );
        assert_eq!(diagnostics, vec![]);

        let diagnostic =
            SymbolResolver::resolve_symbols(&engine, &mut relations, &mut to_visit, &mut visited);

        assert_eq!(
            diagnostic,
            vec![
                Diagnostic::new(
                    DiagnosticID::UnknownSymbol,
                    SourceObjectId(0),
                    "Could not resolve symbol C.",
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

        let mut to_visit = vec![Name::new("test")];
        let mut visited = HashSet::new();

        let diagnostics = SymbolCollector::collect_symbols(
            &mut engine,
            &mut relations,
            &mut to_visit,
            &mut visited,
            &mut importer,
        );

        assert_eq!(diagnostics, vec![]);

        let mut resolver = SymbolResolver::new(&mut engine, &mut relations);
        resolver.resolve_imports(
            SourceObjectId(0),
            UnresolvedImports::default(),
            &mut to_visit,
            &mut visited,
        );
        resolver.resolve_trees().expect("resolution errors");

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
