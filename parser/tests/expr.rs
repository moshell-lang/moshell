use ast::call::{
    Call, Detached, MethodCall, ProgrammaticCall, Redir, RedirFd, RedirOp, Redirected,
};
use ast::control_flow::{For, ForKind, RangeFor};
use ast::group::{Block, Parenthesis};
use ast::lambda::LambdaDef;
use ast::operation::{BinaryOperation, BinaryOperator};
use ast::r#type::{CastedExpr, ParametrizedType, Type};
use ast::range::{Iterable, NumericRange};
use ast::value::{Literal, LiteralValue, TemplateString};
use ast::variable::{Assign, TypedVariable, VarDeclaration, VarKind, VarReference};
use ast::Expr;
use context::source::{Source, SourceSegmentHolder};
use context::str_find::{find_between, find_in, find_in_nth};
use parser::parse;
use parser::source::{literal, literal_nth};
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
    let parsed = parse(source).expect("Failed to parse");

    let expected = vec![Expr::VarDeclaration(VarDeclaration {
        kind: VarKind::Var,
        var: TypedVariable {
            name: "a",
            ty: Some(Type::Parametrized(ParametrizedType {
                path: vec![],
                name: "int",
                params: Vec::new(),
                segment: find_in(source.source, "int"),
            })),
            segment: find_in(source.source, "a:int"),
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
fn expr_cast() {
    let content = "$((1 as Exitcode + 1 as Int)) as Float";
    let source = Source::unknown(content);
    let result = parse(source).expect("parse error");
    assert_eq!(
        result,
        vec![Expr::Casted(CastedExpr {
            expr: Box::new(Expr::Parenthesis(Parenthesis {
                expression: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Casted(CastedExpr {
                        expr: Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in(content, "1"),
                        })),
                        casted_type: Type::Parametrized(ParametrizedType {
                            path: vec![],
                            name: "Exitcode",
                            params: Vec::new(),
                            segment: find_in(content, "Exitcode"),
                        }),
                        segment: find_in(content, "1 as Exitcode"),
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::Casted(CastedExpr {
                        expr: Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in_nth(content, "1", 1),
                        })),
                        casted_type: Type::Parametrized(ParametrizedType {
                            path: vec![],
                            name: "Int",
                            params: Vec::new(),
                            segment: find_in(content, "Int"),
                        }),
                        segment: find_in(content, "1 as Int"),
                    })),
                })),
                segment: find_between(source.source, "$((", "))"),
            })),
            casted_type: Type::Parametrized(ParametrizedType {
                path: vec![],
                name: "Float",
                params: Vec::new(),
                segment: find_in(source.source, "Float"),
            }),
            segment: source.segment(),
        })]
    );
}

#[test]
fn lambda_in_val() {
    let source = Source::unknown("val x = (a) => $a + $b");
    let parsed = parse(source).expect("Failed to parse.");
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
                    segment: find_in_nth(source.source, "a", 1),
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
                segment: find_between(source.source, "(a)", "$b"),
            }))),
            segment: source.segment(),
        })]
    );
}

#[test]
fn lambda_empty_params() {
    let source = Source::unknown("() => $a + $b");
    let parsed = parse(source).expect("Failed to parse.");
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
            segment: source.segment(),
        })]
    );
}

#[test]
fn lambda_in_classic_call() {
    let source = Source::unknown("echo a => $a + $b");
    let parsed = parse(source).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            path: Vec::new(),
            type_parameters: Vec::new(),
            arguments: vec![
                literal(source.source, "echo"),
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
    let parsed = parse(source).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::ProgrammaticCall(ProgrammaticCall {
            path: vec![],
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
                        segment: find_in(source.source, "$n")
                    })),
                    op: BinaryOperator::Times,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: "n",
                        segment: find_in_nth(source.source, "$n", 1)
                    })),
                })),
                segment: find_in(source.source, "n => $n * $n")
            })],
            type_parameters: vec![],
            segment: source.segment(),
        })]
    );
}

#[test]
fn lambda_in_pfc() {
    let source = Source::unknown("calc(() => $a + $b)");
    let parsed = parse(source).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::ProgrammaticCall(ProgrammaticCall {
            path: vec![],
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
                segment: find_in(source.source, "() => $a + $b")
            })],
            type_parameters: vec![],
            segment: source.segment(),
        })]
    );
}

#[test]
fn identity_lambda() {
    let source = Source::unknown("a => $a");
    let parsed = parse(source).expect("Failed to parse.");
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
            segment: source.segment(),
        })]
    );
}

#[test]
fn background_command_echo() {
    let source = Source::unknown("echo hello &\n");
    let parsed = parse(source).expect("Failed to parse");
    let expected = vec![Expr::Detached(Detached {
        underlying: Box::new(Expr::Call(Call {
            path: Vec::new(),
            arguments: vec![
                literal(source.source, "echo"),
                literal(source.source, "hello"),
            ],
            type_parameters: Vec::new(),
        })),
        segment: find_in(source.source, "echo hello &"),
    })];
    assert_eq!(parsed, expected);
}

#[test]
fn command_starting_with_arg() {
    let source = Source::unknown("- W");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            path: Vec::new(),
            arguments: vec![literal(source.source, "-"), literal(source.source, "W")],
            type_parameters: Vec::new()
        })]
    );
}

#[test]
fn constructor_in_call() {
    let source = Source::unknown("echo Foo() Bar\\(\\)");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            path: Vec::new(),
            arguments: vec![
                literal(source.source, "echo"),
                Expr::ProgrammaticCall(ProgrammaticCall {
                    path: vec![],
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
    let parsed = parse(source).expect("Failed to parse");
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
                    path: Vec::new(),
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
                path: Vec::new(),
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
    let parsed = parse(source).expect("Failed to parse");
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
    let parsed = parse(source).expect("Failed to parse");
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
            body: Box::new(Expr::Break(find_in(source.source, "break"))),
            segment: source.segment()
        })]
    );
}

#[test]
fn call_not_assign() {
    let source = Source::unknown("a '=' 5");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            path: Vec::new(),
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
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Assign(Assign {
            name: "a",
            value: Box::new(Expr::ProgrammaticCall(ProgrammaticCall {
                path: vec![],
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
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::ProgrammaticCall(ProgrammaticCall {
            path: vec![],
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
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            path: Vec::new(),
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
fn method_and_function_calls_mixed() {
    let source = Source::unknown("create().foo(dummy().truthy())\n.bar()");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::MethodCall(MethodCall {
            source: Box::new(Expr::MethodCall(MethodCall {
                source: Box::new(Expr::ProgrammaticCall(ProgrammaticCall {
                    path: Vec::new(),
                    name: "create",
                    arguments: Vec::new(),
                    type_parameters: Vec::new(),
                    segment: find_in(source.source, "create()")
                })),
                name: Some("foo"),
                arguments: vec![Expr::MethodCall(MethodCall {
                    source: Box::new(Expr::ProgrammaticCall(ProgrammaticCall {
                        path: Vec::new(),
                        name: "dummy",
                        arguments: Vec::new(),
                        type_parameters: Vec::new(),
                        segment: find_in(source.source, "dummy()")
                    })),
                    name: Some("truthy"),
                    arguments: Vec::new(),
                    type_parameters: Vec::new(),
                    segment: find_in(source.source, ".truthy()")
                })],
                type_parameters: Vec::new(),
                segment: find_in(source.source, ".foo(dummy().truthy())")
            })),
            name: Some("bar"),
            arguments: Vec::new(),
            type_parameters: Vec::new(),
            segment: find_in(source.source, ".bar()")
        })]
    );
}

#[test]
fn block_method_call() {
    let source = Source::unknown("{ $x }.foo('a')");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::MethodCall(MethodCall {
            source: Box::new(Expr::Block(Block {
                expressions: vec![Expr::VarReference(VarReference {
                    name: "x",
                    segment: find_in(source.source, "$x")
                })],
                segment: find_between(source.source, "{", "}")
            })),
            name: Some("foo"),
            arguments: vec![literal(source.source, "'a'")],
            type_parameters: Vec::new(),
            segment: find_between(source.source, ".", ")")
        })]
    );
}

#[test]
fn method_call_with_type_params_and_ref() {
    let source = Source::unknown("$a.foo($d);echo \"${c.bar[T]()}\"");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![
            Expr::MethodCall(MethodCall {
                source: Box::new(Expr::VarReference(VarReference {
                    name: "a",
                    segment: find_in(source.source, "$a")
                })),
                name: Some("foo"),
                arguments: vec![Expr::VarReference(VarReference {
                    name: "d",
                    segment: find_in(source.source, "$d")
                })],
                type_parameters: Vec::new(),
                segment: find_between(source.source, ".", ")")
            }),
            Expr::Call(Call {
                path: Vec::new(),
                arguments: vec![
                    literal(source.source, "echo"),
                    Expr::TemplateString(TemplateString {
                        parts: vec![Expr::MethodCall(MethodCall {
                            source: Box::new(Expr::VarReference(VarReference {
                                name: "c",
                                segment: find_in(source.source, "${c")
                            })),
                            name: Some("bar"),
                            arguments: Vec::new(),
                            type_parameters: vec![Type::Parametrized(ParametrizedType {
                                path: Vec::new(),
                                name: "T",
                                params: Vec::new(),
                                segment: find_in(source.source, "T")
                            })],
                            segment: find_in(source.source, ".bar[T]()")
                        })],
                    }),
                ],
                type_parameters: Vec::new()
            }),
        ]
    );
}

#[test]
fn comments_mix_with_spaces() {
    let source = Source::unknown(
        "{
    //
    //
    ping localhost
}",
    );
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Block(Block {
            expressions: vec![Expr::Call(Call {
                path: Vec::new(),
                arguments: vec![
                    literal(source.source, "ping"),
                    literal(source.source, "localhost")
                ],
                type_parameters: Vec::new()
            })],
            segment: source.segment()
        })]
    );
}

#[test]
fn classic_call_no_regression() {
    let source = Source::unknown("test '=>' ,,here, ->..3 54a2 => 1..=9");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            path: Vec::new(),
            arguments: vec![
                literal(source.source, "test"),
                literal(source.source, "'=>'"),
                literal(source.source, ",,here,"),
                literal(source.source, "->..3"),
                literal(source.source, "54a2"),
                literal_nth(source.source, "=>", 1),
                literal(source.source, "1..=9"),
            ],
            type_parameters: Vec::new()
        })]
    );
}
