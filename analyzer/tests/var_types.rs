use compiler::environment::Environment;
use compiler::{analyze, Diagnostic};
use context::source::Source;

#[test]
fn int_literals_are_const() {
    let source = Source::unknown("1");
    let env = Environment::top_level();
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        env.lookup_type("Int").expect("Failed to lookup type")
    );
}

#[test]
fn const_plus_const_is_const() {
    let source = Source::unknown("$(( 1 + 2 ))");
    let env = Environment::top_level();
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        env.lookup_type("Int").expect("Failed to lookup type")
    );
}

#[test]
fn template_of_const() {
    let source = Source::unknown("val n = 9; val str = \"n = $n\"");
    let env = Environment::top_level();
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        env.lookup_type("Nothing").expect("Failed to lookup type")
    );
}

#[test]
fn empty_is_const() {
    let source = Source::unknown("{}");
    let env = Environment::top_level();
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        env.lookup_type("Nothing").expect("Failed to lookup type")
    );
}

#[test]
fn incompatible_types() {
    let source = Source::unknown("$(( 5 + \"5\" ))");
    let result = analyze(source);
    assert_eq!(
        result,
        Err(vec![Diagnostic {
            message: "Binary operation must have the same type on both sides".to_string(),
        }])
    );
}

#[test]
#[ignore]
fn int_to_float() {
    let source = Source::unknown("val n = 5; $(( n + 6.0 ))");
    let env = Environment::top_level();
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        env.lookup_type("Float").expect("Failed to lookup type")
    );
}

#[test]
fn out_of_scope() {
    let source = Source::unknown("{ val n = 9 }; echo $n");
    let result = analyze(source);
    assert_eq!(
        result,
        Err(vec![Diagnostic {
            message: "Unknown variable: n".to_string(),
        }])
    );
}
