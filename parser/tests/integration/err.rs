use pretty_assertions::assert_eq;

use ast::call::{Call, ProgrammaticCall};
use ast::function::{FunctionDeclaration, FunctionParameter};
use ast::group::Block;
use ast::r#type::{ParametrizedType, Type, TypeParameter};
use ast::r#use::InclusionPathItem;
use ast::value::Literal;
use ast::variable::{TypedVariable, VarDeclaration, VarKind, VarName, VarReference};
use ast::Expr;
use context::source::SourceSegmentHolder;
use context::str_find::{find_in, find_in_nth};
use parser::err::{ParseError, ParseErrorKind, ParseReport};
use parser::parse;
use parser::source::{identifier, identifier_nth, literal, literal_nth};

#[test]
fn repos_delimiter_stack() {
    let source = "{\n\
    val n=$(test $n})\n\
    echo invalid ]\n\
    }\n\
    echo end\n\
    val x=9!3";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![
                Expr::Call(Call {
                    arguments: vec![literal_nth(source, "echo", 1), literal(source, "end")],
                }),
                Expr::VarDeclaration(VarDeclaration {
                    kind: VarKind::Val,
                    var: TypedVariable {
                        name: identifier(source, "x"),
                        ty: None,
                    },
                    initializer: Some(Box::new(Expr::Literal(Literal {
                        parsed: 9.into(),
                        segment: find_in(source, "9")
                    }))),
                    segment: find_in(source, "val x=9")
                })
            ],
            errors: vec![
                ParseError {
                    message: "Mismatched closing delimiter.".to_string(),
                    position: source.find('}').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unpaired(source.find('(').map(|p| p..p + 1).unwrap())
                },
                ParseError {
                    message: "Mismatched closing delimiter.".to_string(),
                    position: source.find(']').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unpaired(source.find('{').map(|p| p..p + 1).unwrap())
                },
                ParseError {
                    message: "expected end of expression or file".to_string(),
                    position: source.rfind('!').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                }
            ],
        }
    );
}

#[test]
fn repos_delimiter_in_var_reference() {
    let source = "echo $(var m = ${..}); echo b";
    let report = parse(source);
    assert_eq!(
        report.errors,
        vec![ParseError {
            message: "Expected variable name.".to_string(),
            position: find_in(source, "..",),
            kind: ParseErrorKind::Unexpected
        }]
    )
}

#[test]
fn what_is_an_import() {
    let source = "{
        use {a, b}
        loop {
            val n =42 as
            break
        } %
        var res = match 42 {
            42 => 21
        }
        res = List(5..;, $a)
    }";
    let report = parse(source);
    assert_eq!(
        report.errors,
        vec![
            ParseError {
                message: "`break` is not a valid type identifier.".to_owned(),
                position: source.find("break").map(|p| p..p + 5).unwrap(),
                kind: ParseErrorKind::Unexpected
            },
            ParseError {
                message: "Expected value".to_owned(),
                position: source.find('%').map(|p| p + 1..p + 2).unwrap(),
                kind: ParseErrorKind::Unexpected
            },
            ParseError {
                message: "Expected value".to_owned(),
                position: source.find(';').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected
            }
        ]
    );
}

#[test]
fn tolerance_in_multiple_groups() {
    let source = "fun f[T, $](x: T, y: +) = $x";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: identifier_nth(source, "f", 1),
                type_parameters: vec![TypeParameter {
                    name: identifier(source, "T"),
                    params: Vec::new(),
                    segment: find_in(source, "T")
                }],
                parameters: vec![FunctionParameter::Named(TypedVariable {
                    name: identifier(source, "x"),
                    ty: Some(Type::Parametrized(ParametrizedType {
                        path: vec![InclusionPathItem::Symbol(identifier_nth(source, "T", 1))],
                        params: vec![],
                        segment: find_in_nth(source, "T", 1)
                    })),
                })],
                return_type: None,
                body: Some(Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("x".into()),
                    segment: find_in(source, "$x")
                }))),
                segment: source.segment()
            })],
            errors: vec![
                ParseError {
                    message: "`$` is not a valid generic type identifier.".to_owned(),
                    position: source.find('$').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "`+` is not a valid type identifier.".to_owned(),
                    position: source.rfind('+').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                }
            ],
        }
    );
}

#[test]
fn invalid_binary_operator_cause_one_error() {
    let source = "val incorrect = 'a' ! 5 + {\n
    }";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: identifier(source, "incorrect"),
                    ty: None,
                },
                initializer: Some(Box::new(literal(source, "'a'"))),
                segment: find_in(source, "val incorrect = 'a'")
            })],
            errors: vec![ParseError {
                message: "expected end of expression or file".to_owned(),
                position: source.find('!').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected
            }],
        }
    );
}

#[test]
fn do_not_accumulate_delimiters() {
    let source = "fun cube(x: List[/[\nStr, Str]]) = {}";
    let report = parse(source);
    assert_eq!(
        report.errors,
        vec![ParseError {
            message: "`/` is not a valid type identifier.".to_owned(),
            position: source.find('/').map(|p| p..p + 1).unwrap(),
            kind: ParseErrorKind::Unexpected
        }]
    );
}

#[test]
fn do_not_accumulate_delimiters2() {
    let source = "fun cube(x: List[)[\nStr, Str]]) = {}";
    let report = parse(source);
    assert_eq!(
        report.errors,
        vec![ParseError {
            message: "Mismatched closing delimiter.".to_owned(),
            position: source.find(')').map(|p| p..p + 1).unwrap(),
            kind: ParseErrorKind::Unpaired(source.find('[').map(|p| p..p + 1).unwrap())
        }]
    );
}

#[test]
fn no_comma_or_two() {
    let source = "fun test[@](a b,,c) = '";
    let mut report = parse(source);
    report.expr.clear();
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![
                ParseError {
                    message: "Unterminated string literal.".to_owned(),
                    position: source.len()..source.len(),
                    kind: ParseErrorKind::Unpaired(source.find('\'').map(|p| p..p + 1).unwrap())
                },
                ParseError {
                    message: "`@` is not a valid generic type identifier.".to_owned(),
                    position: source.find('@').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "A comma or a closing bracket was expected here".to_owned(),
                    position: source.find(" b").map(|p| p + 1..p + 2).unwrap(),
                    kind: ParseErrorKind::Expected("',' or ')'".to_string())
                },
                ParseError {
                    message: "Expected parameter.".to_owned(),
                    position: source.rfind(',').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                }
            ],
        }
    );
}

#[test]
fn single_colon() {
    let source = "use std::foo:{bar, test}";
    let report = parse(source);
    assert_eq!(
        report.errors,
        vec![ParseError {
            message: "Expected `::`.".to_owned(),
            position: source.rfind(':').map(|p| p..p + 1).unwrap(),
            kind: ParseErrorKind::Unexpected
        }]
    );
}

#[test]
fn colon_return_type() {
    let source = "fun foo(): int = {}";
    let report = parse(source);
    assert_eq!(
        report.errors,
        vec![ParseError {
            message: "Return types are denoted using `->`.".to_owned(),
            position: source.find(':').map(|p| p..p + 1).unwrap(),
            kind: ParseErrorKind::Expected("`->`".to_owned())
        }]
    );
}

#[test]
fn multiple_errors_in_parameters() {
    let source = "f(1 + , if true; $, (2 + 3)";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![
                ParseError {
                    message: "Unexpected token ','.".to_owned(),
                    position: source.find(',').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "Expected variable name.".to_owned(),
                    position: source.rfind(',').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "Expected closing parenthesis.".to_owned(),
                    position: source.len()..source.len(),
                    kind: ParseErrorKind::Unpaired(source.find('(').map(|p| p..p + 1).unwrap())
                }
            ],
        }
    );
}

#[test]
fn do_not_self_lock() {
    let source = "fun g[\n, ](, ) = {}";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: identifier(source, "g"),
                parameters: vec![],
                type_parameters: vec![],
                body: Some(Box::new(Expr::Block(Block {
                    expressions: vec![],
                    segment: find_in(source, "{}")
                }))),
                return_type: None,
                segment: source.segment()
            })],
            errors: vec![
                ParseError {
                    message: "Expected type parameter.".to_owned(),
                    position: source.find(',').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "Expected parameter.".to_owned(),
                    position: source.rfind(',').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                }
            ],
        }
    );
}

#[test]
fn list_are_not_tricked_by_blanks() {
    let source = "fun pow[\n";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Expected ']' delimiter.".to_owned(),
                position: source.len()..source.len(),
                kind: ParseErrorKind::Unpaired(source.find('[').map(|p| p..p + 1).unwrap())
            }],
        }
    );
}

#[test]
fn expected_value_found_eof() {
    let source = "val i =\n";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Expected value".to_string(),
                position: source.find('=').map(|p| p + 1..p + 2).unwrap(),
                kind: ParseErrorKind::Unexpected
            }],
        }
    );
}

#[test]
fn expected_value_found_semicolon() {
    let source = "val j =;";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Expected value".to_string(),
                position: source.find(';').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected
            }],
        }
    );
}

#[test]
fn for_no_dollar_help() {
    let source = "for $i in 5..9";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Receiver variables do not start with '$'.".to_string(),
                position: source.find('$').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::UnexpectedInContext(
                    "Consider removing the '$' prefix: for i in 5..9".to_string()
                )
            }],
        }
    );
}

#[test]
fn expected_double_delimiter_for() {
    let source = "for ((i = 0; $i < 10; i+=1); break";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Expected '))' at end of conditional for".to_string(),
                position: source.find(')').map(|p| p + 1..p + 2).unwrap(),
                kind: ParseErrorKind::Unpaired(source.find('(').map(|p| p..p + 2).unwrap())
            }],
        }
    );
}

#[test]
fn expected_double_delimiter_mismatched() {
    let source = "for ((i = 0; ]; i++)); continue";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![Expr::Continue(find_in(source, "continue"))],
            errors: vec![ParseError {
                message: "Mismatched closing delimiter.".to_string(),
                position: source.find(']').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unpaired(source.find('(').map(|p| p + 1..p + 2).unwrap())
            }],
        }
    );
}

#[test]
fn double_comma_parentheses() {
    let source = "Bar('m' , ,)";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![InclusionPathItem::Symbol(identifier(source, "Bar"))],
                arguments: vec![literal(source, "'m'")],
                type_parameters: Vec::new(),
                segment: source.segment(),
            })],
            errors: vec![ParseError {
                message: "Expected argument.".to_string(),
                position: source.rfind(',').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected
            }],
        }
    );
}

#[test]
fn double_comma_function() {
    let source = "fun foo(a: Int, , b: Int) = $a + $b /";
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![
                ParseError {
                    message: "Expected parameter.".to_string(),
                    position: source.rfind(',').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "Expected value".to_string(),
                    position: source.len()..source.len(),
                    kind: ParseErrorKind::Unexpected
                }
            ],
        }
    );
}

#[test]
fn quotes_are_delimiters() {
    let source = "LD_PRELOAD=\"$(dirname $(readlink -f $0))/lib.so\" \"$@\"";
    let report = parse(source);
    assert_eq!(
        report.errors,
        vec![ParseError {
            message: "expected end of expression or file".to_owned(),
            position: source
                .match_indices('"')
                .nth(2)
                .map(|(p, _)| p..p + 1)
                .unwrap(),
            kind: ParseErrorKind::Unexpected
        }]
    );
}

#[test]
fn call_number() {
    let source = "echo 7()";
    let report = parse(source);
    assert_eq!(
        report.errors,
        vec![ParseError {
            message: "expected end of expression or file".to_owned(),
            position: source.find('(').map(|p| p..p + 1).unwrap(),
            kind: ParseErrorKind::Unexpected,
        }]
    );
}

#[test]
fn method_recover_impl() {
    let source = "impl Bar { val count = 0 }";
    let report = parse(source);
    assert_eq!(
        report.errors,
        vec![ParseError {
            message: "expected 'fun' keyword at start of function declaration.".to_owned(),
            position: find_in(source, "val"),
            kind: ParseErrorKind::Unexpected,
        }]
    );
}
