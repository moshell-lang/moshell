use compiler::environment::Environment;
use compiler::{analyze, Diagnostic};
use compiler::builtin_types::{float, int};
use compiler::types::Type;
use context::source::Source;

#[test]
fn int_literals_are_const() {
    let source = Source::unknown("1");
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        Type::Parametrized(int())
    );
}

#[test]
fn const_plus_const_is_const() {
    let source = Source::unknown("$(( 1 + 2 ))");
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        Type::Parametrized(int())
    );
}

#[test]
fn template_of_const() {
    let source = Source::unknown("val n = 9; val str = \"n = $n\"");
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        Type::Nothing
    );
}

#[test]
fn empty_is_const() {
    let source = Source::unknown("{}");
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        Type::Nothing
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
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        Type::Parametrized(float())
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
