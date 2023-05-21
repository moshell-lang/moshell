use checker::engine::Engine;
use checker::environment::variables::Variable;
use checker::importer::StaticImporter;
use checker::name::Name;
use checker::resolver::{GlobalObjectId, Resolver, SourceObjectId, Symbol};
use checker::steps::collect::collect_symbols;
use context::source::Source;
use context::str_find::{find_between, find_in};

#[test]
fn collect_sample() {
    let content = include_str!("debug_sample.msh");
    let source = Source::new(content, "debug_sample.msh");
    let root_name = Name::new("debug_sample");
    let mut engine = Engine::default();
    let mut resolver = Resolver::default();
    let mut importer = StaticImporter::new([(root_name.clone(), source)]);
    collect_symbols(&mut engine, &mut resolver, root_name.clone(), &mut importer).ok();
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

    let lambda_env = engine
        .get_environment(SourceObjectId(3))
        .expect("Unable to get lambda environment");
    let variables = lambda_env.variables.global_vars().collect::<Vec<_>>();
    assert_eq!(variables, vec![(&"n".to_owned(), GlobalObjectId(0))]);
}
