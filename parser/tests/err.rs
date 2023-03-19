use ast::call::Call;
use ast::Expr;
use context::source::Source;
use parser::err::{ParseError, ParseErrorKind, ParseReport};
use parser::parse;

#[test]
fn repos_delimiter_stack() {
    let content = "{\n\
    val n=$(test $n})\n\
    echo invalid ]\n\
    }\n\
    echo end\n\
    val n=9!3";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![Expr::Call(Call {
                arguments: vec![Expr::Literal("echo".into()), Expr::Literal("end".into())],
                type_parameters: Vec::new()
            })],
            errors: vec![
                ParseError {
                    message: "Mismatched closing delimiter.".to_string(),
                    position: content.find('}').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unpaired(content.find('(').map(|p| p..p + 1).unwrap())
                },
                ParseError {
                    message: "invalid infix operator".to_string(),
                    position: content.rfind('!').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                }
            ],
            stack_ended: true,
        }
    );
}

#[test]
fn expected_value_found_eof() {
    let content = "val i =\n";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Expected value".to_string(),
                position: content.find('=').map(|p| p + 1..p + 2).unwrap(),
                kind: ParseErrorKind::Unexpected
            }],
            stack_ended: true,
        }
    );
}

#[test]
fn expected_value_found_semicolon() {
    let content = "val j =;";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Expected value".to_string(),
                position: content.find(';').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected
            }],
            stack_ended: true,
        }
    );
}

#[test]
fn arithmetic_help() {
    let content = "6 + 3 * 9";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Binary operations must be enclosed in a value expression.".to_string(),
                position: 0..content.len(),
                kind: ParseErrorKind::UnexpectedInContext("$(( 6 + 3 * 9 ))".to_string())
            }],
            stack_ended: true,
        }
    );
}

#[test]
fn for_no_dollar_help() {
    let content = "for $i in 5..9";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Receiver variables do not start with '$'.".to_string(),
                position: content.find('$').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::UnexpectedInContext(
                    "Consider removing the '$' prefix: for i in 5..9".to_string()
                )
            }],
            stack_ended: true,
        }
    );
}

#[test]
fn expected_double_delimiter_for() {
    let content = "for ((i = 0; i < 10; i++); break";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Expected '))' at end of conditional for".to_string(),
                position: content.find(')').map(|p| p + 1..p + 2).unwrap(),
                kind: ParseErrorKind::Unpaired(content.find('(').map(|p| p..p + 2).unwrap())
            }],
            stack_ended: false,
        }
    );
}

#[test]
fn double_comma_parentheses() {
    let content = "Bar(4 , ,)";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Expected argument.".to_string(),
                position: content.rfind(',').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected
            }],
            stack_ended: true,
        }
    );
}

#[test]
fn double_comma_function() {
    let content = "fun foo(a: Int, , b: Int) = a + b";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Expected parameter.".to_string(),
                position: content.rfind(',').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected
            }],
            stack_ended: true,
        }
    );
}
