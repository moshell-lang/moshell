use ast::call::{Call, ProgrammaticCall, Redir, RedirFd, RedirOp, Redirected};
use ast::control_flow::{For, ForKind, RangeFor};
use ast::lambda::LambdaDef;
use ast::operation::{BinaryOperation, BinaryOperator};
use ast::r#type::{SimpleType, Type};
use ast::range::{Iterable, NumericRange};
use ast::value::{Literal, LiteralValue};
use ast::variable::{Assign, TypedVariable, VarDeclaration, VarKind, VarReference};
use ast::Expr;
use context::source::Source;
use parser::parse;
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
            ty: Some(Type::Simple(SimpleType {
                name: "int",
                params: Vec::new(),
            })),
        },
        initializer: Some(Box::new(Expr::Literal(Literal {
            lexeme: "1",
            parsed: LiteralValue::Int(1),
        }))),
    })];
    assert_eq!(parsed, expected);
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
            },
            initializer: Some(Box::new(Expr::LambdaDef(LambdaDef {
                args: vec![TypedVariable {
                    name: "a",
                    ty: None,
                },],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference { name: "a" })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::VarReference(VarReference { name: "b" })),
                })),
            }))),
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
            type_parameters: Vec::new(),
            arguments: vec![
                Expr::Literal("echo".into()),
                Expr::Literal("a".into()),
                Expr::Literal("=>".into()),
                Expr::VarReference(VarReference { name: "a" }),
                Expr::Literal("+".into()),
                Expr::VarReference(VarReference { name: "b" }),
            ]
        })]
    );
}

#[test]
fn lambda() {
    let source = Source::unknown("() => $a + $b");
    let parsed = parse(source).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::LambdaDef(LambdaDef {
            args: vec![],
            body: Box::new(Expr::Binary(BinaryOperation {
                left: Box::new(Expr::VarReference(VarReference { name: "a" })),
                op: BinaryOperator::Plus,
                right: Box::new(Expr::VarReference(VarReference { name: "b" })),
            })),
        })]
    );
}

#[test]
fn command_echo() {
    let source = Source::unknown("echo hello");
    let parsed = parse(source).expect("Failed to parse");

    let expected = vec![Expr::Call(Call {
        arguments: vec![Expr::Literal("echo".into()), Expr::Literal("hello".into())],
        type_parameters: Vec::new(),
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
            arguments: vec![Expr::Literal("-".into()), Expr::Literal("W".into())],
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
            arguments: vec![
                Expr::Literal("echo".into()),
                Expr::ProgrammaticCall(ProgrammaticCall {
                    name: "Foo",
                    arguments: vec![],
                    type_parameters: vec![],
                }),
                Expr::Literal(Literal {
                    lexeme: "Bar\\(\\)",
                    parsed: "Bar()".into(),
                }),
            ],
            type_parameters: Vec::new()
        })]
    );
}

#[test]
fn arithmetic_multiple_lines() {
    let parsed = parse(Source::unknown("val n = 1\\\n + 2")).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::VarDeclaration(VarDeclaration {
            kind: VarKind::Val,
            var: TypedVariable {
                name: "n",
                ty: None,
            },
            initializer: Some(Box::new(Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Literal(Literal {
                    lexeme: "1",
                    parsed: 1.into(),
                })),
                op: BinaryOperator::Plus,
                right: Box::new(Expr::Literal(Literal {
                    lexeme: "2",
                    parsed: 2.into(),
                })),
            }))),
        })],
    );
}

#[test]
fn wildcard_redirect_or() {
    let source =
        Source::unknown("docker image inspect moshell:0.1 &> /dev/null || echo 'Unknown image!'");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Binary(BinaryOperation {
            left: Box::new(Expr::Redirected(Redirected {
                expr: Box::new(Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal("docker".into()),
                        Expr::Literal("image".into()),
                        Expr::Literal("inspect".into()),
                        Expr::Literal("moshell:0.1".into()),
                    ],
                    type_parameters: Vec::new()
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Wildcard,
                    operator: RedirOp::Write,
                    operand: Expr::Literal("/dev/null".into()),
                },],
            })),
            op: BinaryOperator::Or,
            right: Box::new(Expr::Call(Call {
                arguments: vec![
                    Expr::Literal("echo".into()),
                    Expr::Literal(Literal {
                        lexeme: "'Unknown image!'",
                        parsed: "Unknown image!".into(),
                    }),
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
                    lexeme: "1",
                    parsed: 1.into(),
                })),
                end: Box::new(Expr::Literal(Literal {
                    lexeme: "10",
                    parsed: 10.into(),
                })),
                step: None,
                upper_inclusive: false,
            }))),
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
                        lexeme: "1",
                        parsed: 1.into(),
                    })),
                    end: Box::new(Expr::Literal(Literal {
                        lexeme: "10",
                        parsed: 10.into(),
                    })),
                    step: Some(Box::new(Expr::Literal(Literal {
                        lexeme: "2",
                        parsed: 2.into(),
                    }))),
                    upper_inclusive: true,
                }))
            })),
            body: Box::new(Expr::Break),
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
            arguments: vec![
                Expr::Literal("a".into()),
                Expr::Literal(Literal {
                    lexeme: "'='",
                    parsed: "=".into(),
                }),
                Expr::Literal(Literal {
                    lexeme: "5",
                    parsed: 5.into(),
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
                name: "Foo",
                arguments: vec![Expr::Literal(Literal {
                    lexeme: "5",
                    parsed: 5.into(),
                })],
                type_parameters: vec![],
            })),
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
            name: "ssh",
            arguments: vec![
                Expr::Literal(Literal {
                    lexeme: "localhost",
                    parsed: "localhost".into(),
                }),
                Expr::Literal(Literal {
                    lexeme: "'ls -l'",
                    parsed: "ls -l".into(),
                }),
                Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        lexeme: "8",
                        parsed: 8.into(),
                    })),
                    op: BinaryOperator::Divide,
                    right: Box::new(Expr::Literal(Literal {
                        lexeme: "2",
                        parsed: 2.into(),
                    })),
                }),
            ],
            type_parameters: Vec::new()
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
            arguments: vec![
                Expr::Literal("ssh".into()),
                Expr::Literal("localhost".into()),
                Expr::Literal(",".into()),
                Expr::Literal(Literal {
                    lexeme: "'ls -l'",
                    parsed: "ls -l".into(),
                }),
                Expr::Literal(Literal {
                    lexeme: "8",
                    parsed: 8.into(),
                }),
                Expr::Literal("/".into()),
                Expr::Literal(Literal {
                    lexeme: "2",
                    parsed: 2.into(),
                }),
            ],
            type_parameters: Vec::new()
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
            arguments: vec![
                Expr::Literal("test".into()),
                Expr::Literal(Literal {
                    lexeme: "'=>'",
                    parsed: "=>".into()
                }),
                Expr::Literal(",,here,".into()),
                Expr::Literal("->..3".into()),
                Expr::Literal("54a2".into()),
                Expr::Literal("=>".into()),
                Expr::Literal("1..=9".into()),
            ],
            type_parameters: Vec::new()
        })]
    );
}
