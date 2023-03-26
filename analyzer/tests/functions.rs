
use compiler::{analyze, Diagnostic};
use compiler::builtin_types::{float, int};
use compiler::types::Type;
use context::source::Source;

#[test]
#[ignore]
fn two_different_environments() {
    let source = Source::unknown(
        "fun f() = {\n\
        val x = 1\n\
        val y = 2\n\
    }\n\
    fun g() = $(( $x + $y ))",
    );
    let result = analyze(source);
    assert_eq!(
        result,
        Err(vec![Diagnostic {
            message: "Unknown variable: x".to_string(),
        }])
    );
}

#[test]
#[ignore]
fn use_global_scope() {
    let source = Source::unknown(
        "val x = 1\n\
        fun g() = $(( $x + $x ))",
    );
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        Type::Nothing
    );
}

#[test]
#[ignore]
fn use_declared_fun() {
    let source = Source::unknown(
        "fun f() -> Int = 9\n\
    f()",
    );
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        Type::Defined(int())
    );
}

#[test]
#[ignore]
fn use_global_fun() {
    let source = Source::unknown(
        "fun f() -> Float = 4.6\n\
    fun g() -> Float = $(( f() + f() ))\n\
    g()",
    );
    let result = analyze(source).expect("Failed to analyze");
    assert_eq!(
        result,
        Type::Defined(float())
    );
}

#[test]
#[ignore]
fn use_non_callable_type() {
    let source = Source::unknown(
        "val x = 9\n\
    x()",
    );
    let result = analyze(source);
    assert_eq!(
        result,
        Err(vec![Diagnostic {
            message: "x is not callable".to_owned(),
        }])
    );
}
