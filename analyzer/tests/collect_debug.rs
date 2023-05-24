use analyzer::engine::Engine;
use analyzer::environment::variables::Variable;
use analyzer::importer::StaticImporter;
use analyzer::name::Name;
use analyzer::relations::{
    GlobalObjectId, Object, Relations, ResolvedSymbol, SourceObjectId, Symbol,
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
    let mut importer = StaticImporter::new(
        [
            (root_name.clone(), source),
            (lib_name, Source::new("val LOG_FILE = 'debug.log'", "lib")),
        ],
        parse_trusted,
    );
    let diagnostics = SymbolCollector::collect_symbols(
        &mut engine,
        &mut relations,
        root_name.clone(),
        &mut importer,
    );
    assert_eq!(diagnostics, vec![]);
    let diagnostics = SymbolResolver::resolve_symbols(&mut engine, &mut relations);
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
    let variables = factorial_env.variables.exported_vars().collect::<Vec<_>>();
    assert_eq!(
        variables,
        vec![
            &Variable::scoped("n".to_owned(), 1),
            &Variable::scoped("a".to_owned(), 2),
            &Variable::scoped("i".to_owned(), 0)
        ]
    );

    let n_parameter = factorial_env
        .variables
        .get("n")
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
    let globals = debug_env.variables.external_vars().collect::<Vec<_>>();
    assert_eq!(globals, vec![(&"LOG_FILE".to_owned(), GlobalObjectId(0))]);
    assert_eq!(
        relations.objects[0],
        Object {
            origin: SourceObjectId(2),
            resolved: Some(ResolvedSymbol {
                module: SourceObjectId(4),
                object_id: 0,
            })
        }
    );

    let lambda_env = engine
        .get_environment(SourceObjectId(3))
        .expect("Unable to get lambda environment");
    let variables = lambda_env.variables.external_vars().collect::<Vec<_>>();
    assert_eq!(variables, vec![(&"n".to_owned(), GlobalObjectId(1))]);
}
