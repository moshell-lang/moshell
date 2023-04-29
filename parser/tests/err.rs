use ast::call::{Call, ProgrammaticCall};
use ast::function::{FunctionDeclaration, FunctionParameter};
use ast::group::Block;
use ast::r#type::{ParametrizedType, Type};
use ast::value::Literal;
use ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
use ast::Expr;
use context::source::{Source, SourceSegmentHolder};
use context::str_find::{find_in, find_in_nth};
use parser::err::{ParseError, ParseErrorKind, ParseReport};
use parser::parse;
use parser::source::{literal, literal_nth};
use pretty_assertions::assert_eq;

#[test]
fn repos_delimiter_stack() {
    let content = "{\n\
    val n=$(test $n})\n\
    echo invalid ]\n\
    }\n\
    echo end\n\
    val x=9!3";
    let source = Source::unknown(content);
    let report = parse(source.clone());
    assert_eq!(
        report,
        ParseReport {
            expr: vec![
                Expr::Call(Call {
                    path: Vec::new(),
                    arguments: vec![
                        literal_nth(source.source, "echo", 1),
                        literal(source.source, "end")
                    ],
                    type_parameters: Vec::new()
                }),
                Expr::VarDeclaration(VarDeclaration {
                    kind: VarKind::Val,
                    var: TypedVariable {
                        name: "x",
                        ty: None,
                        segment: find_in(source.source, "x")
                    },
                    initializer: Some(Box::new(Expr::Literal(Literal {
                        parsed: 9.into(),
                        segment: find_in(source.source, "9")
                    }))),
                    segment: find_in(source.source, "val x=9")
                })
            ],
            errors: vec![
                ParseError {
                    message: "Mismatched closing delimiter.".to_string(),
                    position: content.find('}').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unpaired(content.find('(').map(|p| p..p + 1).unwrap())
                },
                ParseError {
                    message: "Mismatched closing delimiter.".to_string(),
                    position: content.find(']').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unpaired(content.find('{').map(|p| p..p + 1).unwrap())
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
fn repos_delimiter_in_var_reference() {
    let content = "echo $(var m = ${..}); echo b";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report.errors,
        vec![ParseError {
            message: "variable reference with empty name".to_string(),
            position: find_in(content, "..",),
            kind: ParseErrorKind::Unexpected
        }]
    )
}

#[test]
fn what_is_an_import() {
    let content = "{
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
    let source = Source::unknown(content);
    let report = parse(source.clone());
    assert_eq!(
        report.errors,
        vec![
            ParseError {
                message: "'break' is not a valid type identifier.".to_owned(),
                position: content.find("break").map(|p| p..p + 5).unwrap(),
                kind: ParseErrorKind::Unexpected
            },
            ParseError {
                message: "invalid expression operator".to_owned(),
                position: content.find('%').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected
            },
            ParseError {
                message: "Expected value".to_owned(),
                position: content.find(';').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected
            }
        ]
    );
}

#[test]
fn tolerance_in_multiple_groups() {
    let content = "fun f[T, $](x: T, y: +) = $x";
    let source = Source::unknown(content);
    let report = parse(source.clone());
    assert_eq!(
        report,
        ParseReport {
            expr: vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: "f",
                type_parameters: vec![Type::Parametrized(ParametrizedType {
                    path: vec![],
                    name: "T",
                    params: vec![],
                    segment: find_in(&source.source, "T")
                })],
                parameters: vec![FunctionParameter::Named(TypedVariable {
                    name: "x",
                    ty: Some(Type::Parametrized(ParametrizedType {
                        path: vec![],
                        name: "T",
                        params: vec![],
                        segment: find_in_nth(&source.source, "T", 1)
                    })),
                    segment: find_in(&source.source, "x: T")
                })],
                return_type: None,
                body: Box::new(Expr::VarReference(VarReference {
                    name: "x",
                    segment: find_in(&source.source, "$x")
                })),
                segment: source.segment()
            })],
            errors: vec![
                ParseError {
                    message: "'$' is not a valid type identifier.".to_owned(),
                    position: content.find('$').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "'+' is not a valid type identifier.".to_owned(),
                    position: content.rfind('+').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                }
            ],
            stack_ended: true,
        }
    );
}

#[test]
fn invalid_binary_operator_cause_one_error() {
    let content = "val incorrect = 'a' ! 5 + {\n
    }";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: "incorrect",
                    ty: None,
                    segment: find_in(content, "incorrect")
                },
                initializer: Some(Box::new(literal(content, "'a'"))),
                segment: find_in(content, "val incorrect = 'a'")
            })],
            errors: vec![ParseError {
                message: "invalid infix operator".to_owned(),
                position: content.find('!').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected
            }],
            stack_ended: true,
        }
    );
}

#[test]
fn do_not_accumulate_delimiters() {
    let content = "fun cube(x: List[/[\nStr, Str]]) = {}";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report.errors,
        vec![ParseError {
            message: "'/' is not a valid type identifier.".to_owned(),
            position: content.find('/').map(|p| p..p + 1).unwrap(),
            kind: ParseErrorKind::Unexpected
        }]
    );
}

#[test]
fn do_not_accumulate_delimiters2() {
    let content = "fun cube(x: List[)[\nStr, Str]]) = {}";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report.errors,
        vec![ParseError {
            message: "Mismatched closing delimiter.".to_owned(),
            position: content.find(')').map(|p| p..p + 1).unwrap(),
            kind: ParseErrorKind::Unpaired(content.find('[').map(|p| p..p + 1).unwrap())
        }]
    );
}

#[test]
fn no_comma_or_two() {
    let content = "fun test[@](a b,,c) = '";
    let source = Source::unknown(content);
    let report = parse(source.clone());
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![
                ParseError {
                    message: "'@' is not a valid type identifier.".to_owned(),
                    position: content.find('@').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "Expected ','".to_owned(),
                    position: content.find(" b").map(|p| p + 1..p + 2).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "Expected parameter.".to_owned(),
                    position: content.rfind(',').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "Unterminated string literal.".to_owned(),
                    position: content.len()..content.len(),
                    kind: ParseErrorKind::Unpaired(content.find('\'').map(|p| p..p + 1).unwrap())
                }
            ],
            stack_ended: true,
        }
    );
}

#[test]
fn multiple_errors_in_parameters() {
    let content = "f(1 + , if true; $, (2 + 3)";
    let source = Source::unknown(content);
    let report = parse(source.clone());
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![
                ParseError {
                    message: "Unexpected token ','.".to_owned(),
                    position: content.find(',').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "variable reference with empty name".to_owned(),
                    position: content.find('$').map(|p| p + 1..p + 2).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "Expected closing parenthesis.".to_owned(),
                    position: content.len()..content.len(),
                    kind: ParseErrorKind::Unpaired(content.find('(').map(|p| p..p + 1).unwrap())
                }
            ],
            stack_ended: false,
        }
    );
}

#[test]
fn do_not_self_lock() {
    let content = "fun g[\n, ](, ) = {}";
    let source = Source::unknown(content);
    let report = parse(source.clone());
    assert_eq!(
        report,
        ParseReport {
            expr: vec![Expr::FunctionDeclaration(FunctionDeclaration {
                name: "g",
                parameters: vec![],
                type_parameters: vec![],
                body: Box::new(Expr::Block(Block {
                    expressions: vec![],
                    segment: find_in(source.source, "{}")
                })),
                return_type: None,
                segment: source.segment()
            })],
            errors: vec![
                ParseError {
                    message: "Expected value.".to_owned(),
                    position: content.find(',').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "expected types".to_owned(),
                    position: content.find(']').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Expected("<types>".to_owned())
                },
                ParseError {
                    message: "Expected parameter.".to_owned(),
                    position: content.rfind(',').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                }
            ],
            stack_ended: true,
        }
    );
}

#[test]
fn list_are_not_tricked_by_blanks() {
    let content = "fun pow[\n";
    let source = Source::unknown(content);
    let report = parse(source.clone());
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![ParseError {
                message: "Expected ']' delimiter.".to_owned(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unpaired(content.find('[').map(|p| p..p + 1).unwrap())
            }],
            stack_ended: false,
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
fn expected_double_delimiter_mismatched() {
    let content = "for ((i = 0; ]; i++)); continue";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![Expr::Continue(find_in(content, "continue"))],
            errors: vec![ParseError {
                message: "Mismatched closing delimiter.".to_string(),
                position: content.find(']').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unpaired(content.find('(').map(|p| p + 1..p + 2).unwrap())
            }],
            stack_ended: true,
        }
    );
}

#[test]
fn double_comma_parentheses() {
    let content = "Bar(m , ,)";
    let source = Source::unknown(content);
    let report = parse(source.clone());
    assert_eq!(
        report,
        ParseReport {
            expr: vec![Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![],
                name: "Bar",
                arguments: vec![literal(content, "m")],
                type_parameters: Vec::new(),
                segment: source.segment(),
            })],
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
    let content = "fun foo(a: Int, , b: Int) = $a + $b";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![],
            errors: vec![
                ParseError {
                    message: "Expected parameter.".to_string(),
                    position: content.rfind(',').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                },
                ParseError {
                    message: "invalid expression operator".to_string(),
                    position: content.rfind('+').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                }
            ],
            stack_ended: true,
        }
    );
}
