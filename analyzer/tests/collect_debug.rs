use analyzer::engine::Engine;
use analyzer::environment::variables::Variable;
use analyzer::importer::StaticImporter;
use analyzer::name::Name;
use analyzer::relations::{
    GlobalObjectId, Object, ObjectState, Relations, ResolvedSymbol, SourceObjectId, Symbol,
};
use analyzer::steps::collect::SymbolCollector;
use analyzer::steps::resolve::SymbolResolver;
use context::source::Source;
use context::str_find::{find_between, find_in};
use parser::parse_trusted;
use pretty_assertions::assert_eq;
use std::collections::HashSet;

#[test]
fn collect_sample() {
    let content = include_str!("debug_sample.msh");
    let source = Source::new(content, "debug_sample.msh");
    let root_name = Name::new("debug_sample");
    let lib_name = Name::new("lib");
    let mut engine = Engine::default();
    let mut relations = Relations::default();

    let mut to_visit = vec![root_name.clone()];
    let mut visited = HashSet::new();
    let mut importer = StaticImporter::new(
        [
            (root_name.clone(), source),
            (
                lib_name,
                Source::new("val LOG_FILE = 'debug.log'; val n", "lib"),
            ),
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
    let diagnostics =
        SymbolResolver::resolve_symbols(&mut engine, &mut relations, &mut to_visit, &mut visited);
    assert_eq!(diagnostics, vec![]);
    let root_env = engine
        .get_environment(SourceObjectId(0))
        .expect("Unable to get root environment");
    assert_eq!(
        root_env.get_raw_symbol(find_between(content, "fun factorial(", "return $a\n}")),
        Some(Symbol::Local(0))
    );
    assert_eq!(
        root_env.get_raw_symbol(find_between(content, "fun debug(", "wait\n}")),
        Some(Symbol::Local(1))
    );

    let factorial_env = engine
        .get_environment(SourceObjectId(1))
        .expect("Unable to get factorial environment");
    assert_eq!(factorial_env.fqn, root_name.child("factorial"));
    let variables = factorial_env.variables.all_vars();
    assert_eq!(
        variables,
        &vec![
            Variable::scoped("n".to_owned(), 0),
            Variable::scoped("a".to_owned(), 1),
            Variable::scoped("i".to_owned(), -2),
        ]
    );
    let exported = factorial_env.variables.exported_vars().collect::<Vec<_>>();
    assert_eq!(exported, Vec::<&Variable>::new());

    let n_parameter = factorial_env
        .variables
        .get_reachable("n")
        .map(Symbol::Local)
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
        .get_environment(SourceObjectId(2))
        .expect("Unable to get debug() environment");
    assert_eq!(debug_env.fqn, root_name.child("debug"));
    let usages = debug_env.variables.external_usages().collect::<Vec<_>>();
    assert_eq!(usages, vec![(&Name::new("LOG_FILE"), GlobalObjectId(0))]);
    assert_eq!(
        relations.objects[0],
        Object {
            origin: SourceObjectId(2),
            state: ObjectState::Resolved(ResolvedSymbol {
                source: SourceObjectId(6),
                object_id: 0,
            }),
        }
    );

    let callback_env = engine
        .get_environment(SourceObjectId(4))
        .expect("Unable to get callback environment");
    assert_eq!(callback_env.fqn, root_name.child("main").child("callback"));
    let globals = callback_env.variables.external_usages().collect::<Vec<_>>();
    assert_eq!(
        globals,
        vec![
            (&Name::new("count"), GlobalObjectId(1)),
            (&Name::new("factorial"), GlobalObjectId(2)),
            (&Name::new("n"), GlobalObjectId(3)),
        ]
    );
    assert_eq!(
        relations.objects[1],
        Object {
            origin: SourceObjectId(4),
            state: ObjectState::Resolved(ResolvedSymbol {
                source: SourceObjectId(3),
                object_id: 0,
            }),
        }
    );
    assert_eq!(
        relations.objects[2],
        Object {
            origin: SourceObjectId(4),
            state: ObjectState::Resolved(ResolvedSymbol {
                source: SourceObjectId(0),
                object_id: 0,
            }),
        }
    );
    assert_eq!(
        relations.objects[3],
        Object {
            origin: SourceObjectId(4),
            state: ObjectState::Resolved(ResolvedSymbol {
                source: SourceObjectId(0),
                object_id: 3,
            }),
        }
    );

    let lambda_env = engine
        .get_environment(SourceObjectId(5))
        .expect("Unable to get lambda environment");
    let variables = lambda_env.variables.external_usages().collect::<Vec<_>>();
    assert_eq!(variables, vec![(&Name::new("n"), GlobalObjectId(4))]);
}
