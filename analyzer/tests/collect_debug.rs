use std::collections::HashSet;

use analyzer::analyze;
use analyzer::engine::Engine;
use pretty_assertions::assert_eq;

use analyzer::environment::symbols::{Symbol, SymbolLocation, SymbolRegistry};
use analyzer::importer::StaticImporter;
use analyzer::imports::Imports;
use analyzer::name::Name;
use analyzer::reef::{Externals, Reef, ReefId};
use analyzer::relations::{
    LocalId, Relation, RelationId, RelationState, Relations, ResolvedSymbol, SourceId, SymbolRef,
};
use analyzer::steps::collect::SymbolCollector;
use analyzer::steps::resolve::SymbolResolver;
use context::source::Source;
use context::str_find::{find_between, find_in};
use parser::parse_trusted;

#[test]
fn collect_sample() {
    let content = include_str!("debug_sample.msh");
    let source = Source::new(content, "debug_sample.msh");
    let root_name = Name::new("debug_sample");
    let lib_name = Name::new("lib");

    let mut engine = Engine::default();
    let mut relations = Relations::default();
    let mut imports = Imports::default();

    let mut to_visit = vec![root_name.clone()];
    let mut visited = HashSet::new();
    let mut importer = StaticImporter::new(
        [
            (root_name.clone(), source),
            (
                lib_name.clone(),
                Source::new("val LOG_FILE = 'debug.log'; val n = 1", "lib"),
            ),
        ],
        parse_trusted,
    );

    let mut externals = Externals::default();
    let lib_reef = Reef::new(
        "lib".to_owned(),
        analyze(lib_name, &mut importer, &externals),
    );
    externals.register(lib_reef);

    let diagnostics = SymbolCollector::collect_symbols(
        &mut engine,
        &mut relations,
        &mut imports,
        &externals,
        &mut to_visit,
        &mut visited,
        &mut importer,
    );
    assert_eq!(diagnostics, vec![]);

    let diagnostics = SymbolResolver::resolve_symbols(
        &mut engine,
        &mut relations,
        &mut imports,
        &externals,
        &mut to_visit,
        &mut visited,
    );
    assert_eq!(diagnostics, vec![]);

    let root_env = engine
        .get_environment(SourceId(0))
        .expect("Unable to get root environment");
    assert_eq!(
        root_env.get_raw_symbol(find_between(content, "fun factorial(", "return $a\n}")),
        Some(SymbolRef::Local(LocalId(0)))
    );
    assert_eq!(
        root_env.get_raw_symbol(find_between(content, "fun debug(", "wait\n}")),
        Some(SymbolRef::Local(LocalId(1)))
    );

    let factorial_env = engine
        .get_environment(SourceId(1))
        .expect("Unable to get factorial environment");
    assert_eq!(factorial_env.fqn, root_name.child("factorial"));
    let variables = factorial_env.symbols.all();
    assert_eq!(
        variables,
        &vec![
            Symbol::scoped("n".to_owned(), 0),
            Symbol::scoped("a".to_owned(), 1),
            Symbol::scoped("i".to_owned(), -2),
        ]
    );
    let exported = factorial_env.symbols.exported_symbols().collect::<Vec<_>>();
    assert_eq!(exported, vec![]);

    let n_parameter = factorial_env
        .symbols
        .find_reachable("n", SymbolRegistry::Objects)
        .map(SymbolRef::Local)
        .expect("Unable to get n symbol");

    assert_eq!(
        factorial_env.get_raw_symbol(find_in(content, "$n")),
        Some(n_parameter)
    );
    let references = {
        let mut references = factorial_env.find_references(n_parameter);
        references.sort_by_key(|range| range.start);
        references
    };
    assert_eq!(
        references,
        vec![find_in(content, "n: Int"), find_in(content, "$n")]
    );

    let debug_env = engine
        .get_environment(SourceId(2))
        .expect("Unable to get debug() environment");
    assert_eq!(debug_env.fqn, root_name.child("debug"));
    let usages = debug_env.symbols.external_symbols().collect::<Vec<_>>();
    assert_eq!(
        usages,
        vec![(
            &SymbolLocation::unspecified(Name::new("LOG_FILE")),
            RelationId(1)
        )]
    );
    assert_eq!(
        relations[RelationId(1)],
        Relation {
            origin: SourceId(2),
            state: RelationState::Resolved(ResolvedSymbol {
                reef: ReefId(1),
                source: SourceId(0),
                object_id: LocalId(0),
            }),
            registry: SymbolRegistry::Objects
        }
    );

    let callback_env = engine
        .get_environment(SourceId(4))
        .expect("Unable to get callback environment");
    assert_eq!(callback_env.fqn, root_name.child("main").child("callback"));

    let mut globals = callback_env.symbols.external_symbols().collect::<Vec<_>>();
    globals.sort_by_key(|(loc, _)| &loc.name);
    assert_eq!(
        globals,
        vec![
            (
                &SymbolLocation::unspecified(Name::new("count")),
                RelationId(2)
            ),
            (
                &SymbolLocation::unspecified(Name::new("factorial")),
                RelationId(3)
            ),
            (&SymbolLocation::unspecified(Name::new("n")), RelationId(4)),
        ]
    );

    let reef_id = ReefId(2);

    assert_eq!(
        relations[RelationId(2)],
        Relation {
            origin: SourceId(4),
            state: RelationState::Resolved(ResolvedSymbol {
                reef: reef_id,
                source: SourceId(3),
                object_id: LocalId(0),
            }),
            registry: SymbolRegistry::Objects
        }
    );
    assert_eq!(
        relations[RelationId(3)],
        Relation {
            origin: SourceId(4),
            state: RelationState::Resolved(ResolvedSymbol {
                reef: reef_id,
                source: SourceId(0),
                object_id: LocalId(0),
            }),
            registry: SymbolRegistry::Objects
        }
    );
    assert_eq!(
        relations[RelationId(4)],
        Relation {
            origin: SourceId(4),
            state: RelationState::Resolved(ResolvedSymbol {
                reef: reef_id,
                source: SourceId(0),
                object_id: LocalId(3),
            }),
            registry: SymbolRegistry::Objects
        }
    );

    let lambda_env = engine
        .get_environment(SourceId(5))
        .expect("Unable to get lambda environment");

    let variables = lambda_env.symbols.external_symbols().collect::<Vec<_>>();
    assert_eq!(
        variables,
        vec![(&SymbolLocation::unspecified(Name::new("n")), RelationId(5))]
    );
}
