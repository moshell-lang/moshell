use ast::call::{Call, ProgrammaticCall, Redir, RedirFd, RedirOp, Redirected};
use ast::control_flow::{For, ForKind, RangeFor};
use ast::lambda::LambdaDef;
use ast::operation::{BinaryOperation, BinaryOperator};
use ast::r#type::{SimpleType, Type};
use ast::range::{Iterable, NumericRange};
use ast::value::{Literal, LiteralValue};
use ast::variable::{Assign, TypedVariable, VarDeclaration, VarKind, VarReference};
use ast::Expr;
use context::source::{Source, SourceSegmentHolder};
use parser::parse;
use parser::source::{find_in, find_in_nth, literal};
use pretty_assertions::assert_eq;

#[test]
fn empty() {
    let source = Source::unknown("");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(parsed, vec![]);
}

#[test]
fn variable_type_and_initializer() {
    let source = Source::unknown("var a:int=1");
    let parsed = parse(source.clone()).expect("Failed to parse");

    let expected = vec![Expr::VarDeclaration(VarDeclaration {
        kind: VarKind::Var,
        var: TypedVariable {
            name: "a",
            ty: Some(Type::Simple(SimpleType {
                name: "int",
                params: Vec::new(),
                segment: find_in(source.source, "int"),
            })),
            segment: find_in(source.source, "a"),
        },
        initializer: Some(Box::new(Expr::Literal(Literal {
            parsed: LiteralValue::Int(1),
            segment: find_in(source.source, "1"),
        }))),
        segment: source.segment(),
    })];
    assert_eq!(parsed, expected);
}

#[test]
fn lambda_in_val() {
    let source = Source::unknown("val x = (a) => $a + $b");
    let parsed = parse(source.clone()).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::VarDeclaration(VarDeclaration {
            kind: VarKind::Val,
            var: TypedVariable {
                name: "x",
                ty: None,
                segment: find_in(source.source, "x"),
            },
            initializer: Some(Box::new(Expr::LambdaDef(LambdaDef {
                args: vec![TypedVariable {
                    name: "a",
                    ty: None,
                    segment: find_in(source.source, "a"),
                },],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: "a",
                        segment: find_in(source.source, "$a")
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: "b",
                        segment: find_in(source.source, "$b")
                    })),
                })),
            }))),
            segment: source.segment(),
        })]
    );
}

#[test]
fn lambda_empty_params() {
    let source = Source::unknown("() => $a + $b");
    let parsed = parse(source.clone()).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::LambdaDef(LambdaDef {
            args: vec![],
            body: Box::new(Expr::Binary(BinaryOperation {
                left: Box::new(Expr::VarReference(VarReference {
                    name: "a",
                    segment: find_in(source.source, "$a")
                })),
                op: BinaryOperator::Plus,
                right: Box::new(Expr::VarReference(VarReference {
                    name: "b",
                    segment: find_in(source.source, "$b")
                })),
            })),
        })]
    );
}

#[test]
fn lambda_in_classic_call() {
    let source = Source::unknown("echo a => $a + $b");
    let parsed = parse(source.clone()).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            type_parameters: Vec::new(),
            arguments: vec![
                literal(source.source, "a"),
                literal(source.source, "=>"),
                Expr::VarReference(VarReference {
                    name: "a",
                    segment: find_in(source.source, "$a")
                }),
                literal(source.source, "+"),
                Expr::VarReference(VarReference {
                    name: "b",
                    segment: find_in(source.source, "$b")
                }),
            ]
        })]
    );
}

#[test]
fn lambda_one_arg() {
    let source = Source::unknown("calc(n => $n * $n)");
    let parsed = parse(source.clone()).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::ProgrammaticCall(ProgrammaticCall {
            name: "calc",
            arguments: vec![Expr::LambdaDef(LambdaDef {
                args: vec![TypedVariable {
                    name: "n",
                    ty: None,
                    segment: find_in(source.source, "n"),
                }],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: "n",
                        segment: find_in_nth(source.source, "$n", 1)
                    })),
                    op: BinaryOperator::Times,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: "n",
                        segment: find_in_nth(source.source, "$n", 2)
                    })),
                })),
            })],
            type_parameters: vec![],
            segment: source.segment(),
        })]
    );
}

#[test]
fn lambda_in_pfc() {
    let source = Source::unknown("calc(() => $a + $b)");
    let parsed = parse(source.clone()).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::ProgrammaticCall(ProgrammaticCall {
            name: "calc",
            arguments: vec![Expr::LambdaDef(LambdaDef {
                args: vec![],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: "a",
                        segment: find_in(source.source, "$a")
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: "b",
                        segment: find_in(source.source, "$b")
                    })),
                })),
            })],
            type_parameters: vec![],
            segment: source.segment(),
        })]
    );
}

#[test]
fn identity_lambda() {
    let source = Source::unknown("a => $a");
    let parsed = parse(source.clone()).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::LambdaDef(LambdaDef {
            args: vec![TypedVariable {
                name: "a",
                ty: None,
                segment: 0..1,
            }],
            body: Box::new(Expr::VarReference(VarReference {
                name: "a",
                segment: find_in(source.source, "$a")
            })),
        })]
    );
}

#[test]
fn command_echo() {
    let source = Source::unknown("echo hello");
    let parsed = parse(source.clone()).expect("Failed to parse");

    let expected = vec![Expr::Call(Call {
        arguments: vec![
            literal(source.source, "echo"),
            literal(source.source, "hello"),
        ],
        type_parameters: Vec::new(),
    })];
    assert_eq!(parsed, expected);
}

#[test]
fn command_starting_with_arg() {
    let source = Source::unknown("- W");
    let parsed = parse(source.clone()).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![literal(source.source, "-"), literal(source.source, "W")],
            type_parameters: Vec::new()
        })]
    );
}

#[test]
fn constructor_in_call() {
    let source = Source::unknown("echo Foo() Bar\\(\\)");
    let parsed = parse(source.clone()).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source.source, "echo"),
                Expr::ProgrammaticCall(ProgrammaticCall {
                    name: "Foo",
                    arguments: vec![],
                    type_parameters: vec![],
                    segment: find_in(source.source, "Foo()"),
                }),
                Expr::Literal(Literal {
                    parsed: "Bar()".into(),
                    segment: find_in(source.source, "Bar\\(\\)")
                }),
            ],
            type_parameters: Vec::new()
        })]
    );
}

#[test]
fn arithmetic_multiple_lines() {
    let source = Source::unknown("val n = 1\\\n + 2");
    let parsed = parse(source.clone()).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::VarDeclaration(VarDeclaration {
            kind: VarKind::Val,
            var: TypedVariable {
                name: "n",
                ty: None,
                segment: find_in(source.source, "n"),
            },
            initializer: Some(Box::new(Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Literal(Literal {
                    parsed: 1.into(),
                    segment: find_in(source.source, "1"),
                })),
                op: BinaryOperator::Plus,
                right: Box::new(Expr::Literal(Literal {
                    parsed: 2.into(),
                    segment: find_in(source.source, "2"),
                })),
            }))),
            segment: 0..source.source.len(),
        })],
    );
}

#[test]
fn wildcard_redirect_or() {
    let content = "docker image inspect moshell:0.1 &> /dev/null || echo 'Unknown image!'";
    let source = Source::unknown(content);
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Binary(BinaryOperation {
            left: Box::new(Expr::Redirected(Redirected {
                expr: Box::new(Expr::Call(Call {
                    arguments: vec![
                        literal(content, "docker"),
                        literal(content, "image"),
                        literal(content, "inspect"),
                        literal(content, "moshell:0.1"),
                    ],
                    type_parameters: Vec::new()
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Wildcard,
                    operator: RedirOp::Write,
                    operand: literal(content, "/dev/null"),
                    segment: find_in(content, "&> /dev/null"),
                }],
            })),
            op: BinaryOperator::Or,
            right: Box::new(Expr::Call(Call {
                arguments: vec![
                    literal(content, "echo"),
                    literal(content, "'Unknown image!'"),
                ],
                type_parameters: Vec::new()
            })),
        })]
    );
}

#[test]
fn assign_iterable() {
    let source = Source::unknown("it = 1..10");
    let parsed = parse(source.clone()).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Assign(Assign {
            name: "it",
            value: Box::new(Expr::Range(Iterable::Range(NumericRange {
                start: Box::new(Expr::Literal(Literal {
                    parsed: 1.into(),
                    segment: find_in(source.source, "1")
                })),
                end: Box::new(Expr::Literal(Literal {
                    parsed: 10.into(),
                    segment: find_in(source.source, "10")
                })),
                step: None,
                upper_inclusive: false,
            }))),
            segment: source.segment()
        })]
    );
}

#[test]
fn for_in_step_2_range() {
    let source = Source::unknown("for i in 1..=10..2; break");
    let parsed = parse(source.clone()).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::For(For {
            kind: Box::new(ForKind::Range(RangeFor {
                receiver: "i",
                iterable: Expr::Range(Iterable::Range(NumericRange {
                    start: Box::new(Expr::Literal(Literal {
                        parsed: 1.into(),
                        segment: find_in(source.source, "1")
                    })),
                    end: Box::new(Expr::Literal(Literal {
                        parsed: 10.into(),
                        segment: find_in(source.source, "10")
                    })),
                    step: Some(Box::new(Expr::Literal(Literal {
                        parsed: 2.into(),
                        segment: find_in(source.source, "2")
                    }))),
                    upper_inclusive: true,
                })),
                segment: find_in(source.source, "i in 1..=10..2")
            })),
            body: Box::new(Expr::Break),
            segment: source.segment()
        })]
    );
}

#[test]
fn call_not_assign() {
    let source = Source::unknown("a '=' 5");
    let parsed = parse(source.clone()).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source.source, "a"),
                literal(source.source, "'='"),
                Expr::Literal(Literal {
                    parsed: 5.into(),
                    segment: find_in(source.source, "5")
                }),
            ],
            type_parameters: Vec::new()
        })]
    );
}

#[test]
fn constructor_assign() {
    let source = Source::unknown("a = Foo(5)");
    let parsed = parse(source.clone()).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Assign(Assign {
            name: "a",
            value: Box::new(Expr::ProgrammaticCall(ProgrammaticCall {
                name: "Foo",
                arguments: vec![Expr::Literal(Literal {
                    parsed: 5.into(),
                    segment: find_in(source.source, "5")
                })],
                type_parameters: vec![],
                segment: find_in(source.source, "Foo(5)")
            })),
            segment: source.segment()
        })]
    );
}

#[test]
fn programmatic_call() {
    let source = Source::unknown("ssh(localhost, 'ls -l', 8 / 2)");
    let parsed = parse(source.clone()).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::ProgrammaticCall(ProgrammaticCall {
            name: "ssh",
            arguments: vec![
                literal(source.source, "localhost"),
                literal(source.source, "'ls -l'"),
                Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: 8.into(),
                        segment: find_in(source.source, "8")
                    })),
                    op: BinaryOperator::Divide,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 2.into(),
                        segment: find_in(source.source, "2")
                    })),
                }),
            ],
            type_parameters: Vec::new(),
            segment: source.segment()
        })]
    );
}

#[test]
fn classic_call() {
    let source = Source::unknown("ssh localhost , 'ls -l' 8 / 2");
    let parsed = parse(source.clone()).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source.source, "ssh"),
                literal(source.source, "localhost"),
                literal(source.source, ","),
                literal(source.source, "'ls -l'"),
                Expr::Literal(Literal {
                    parsed: 8.into(),
                    segment: find_in(source.source, "8")
                }),
                literal(source.source, "/"),
                Expr::Literal(Literal {
                    parsed: 2.into(),
                    segment: find_in(source.source, "2")
                }),
            ],
            type_parameters: Vec::new()
        })]
    );
}

#[test]
fn classic_call_no_regression() {
    let source = Source::unknown("test '=>' ,,here, ->..3 54a2 => 1..=9");
    let parsed = parse(source.clone()).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source.source, "test"),
                literal(source.source, "'=>'"),
                literal(source.source, ",,here,"),
                literal(source.source, "->..3"),
                literal(source.source, "54a2"),
                literal(source.source, "=>"),
                literal(source.source, "1..=9"),
            ],
            type_parameters: Vec::new()
        })]
    );
}
