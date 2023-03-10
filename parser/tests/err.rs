use context::source::Source;
use parser::ast::callable::Call;
use parser::ast::Expr;
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
fn excepted_value_found_eof() {
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
fn excepted_value_found_semicolon() {
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
    let source = Source::unknown("6 + 3 * 9");
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
