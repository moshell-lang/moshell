use pretty_assertions::assert_eq;

use ast::call::{
    Call, Detached, MethodCall, ProgrammaticCall, Redir, RedirFd, RedirOp, Redirected,
};
use ast::control_flow::{For, ForKind, If, RangeFor};
use ast::function::Return;
use ast::group::{Block, Parenthesis};
use ast::lambda::LambdaDef;
use ast::operation::{BinaryOperation, BinaryOperator, UnaryOperation, UnaryOperator};
use ast::r#type::{CastedExpr, ParametrizedType, Type};
use ast::r#use::InclusionPathItem;
use ast::range::{Iterable, NumericRange};
use ast::value::{Literal, LiteralValue, TemplateString};
use ast::variable::{
    Assign, AssignOperator, Path, TypedVariable, VarDeclaration, VarKind, VarName, VarReference,
};
use ast::Expr;
use context::source::SourceSegmentHolder;
use context::str_find::{find_between, find_in, find_in_nth};
use parser::parse;
use parser::source::{identifier, identifier_nth, literal, literal_nth};

#[test]
fn empty() {
    let source = "";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(parsed, vec![]);
}

#[test]
fn variable_type_and_initializer() {
    let source = "var a:int=1";
    let parsed = parse(source).expect("Failed to parse");

    let expected = vec![Expr::VarDeclaration(VarDeclaration {
        kind: VarKind::Var,
        var: TypedVariable {
            name: identifier_nth(source, "a", 1),
            ty: Some(Type::Parametrized(ParametrizedType {
                path: vec![InclusionPathItem::Symbol(identifier(source, "int"))],
                params: Vec::new(),
                segment: find_in(source, "int"),
            })),
        },
        initializer: Some(Box::new(Expr::Literal(Literal {
            parsed: LiteralValue::Int(1),
            segment: find_in(source, "1"),
        }))),
        segment: source.segment(),
    })];
    assert_eq!(parsed, expected);
}

#[test]
fn expr_cast() {
    let source = "$((1 as Exitcode + -1 as Int)) as Float";
    let result = parse(source).expect("parse error");
    assert_eq!(
        result,
        vec![Expr::Casted(CastedExpr {
            expr: Box::new(Expr::Parenthesis(Parenthesis {
                expression: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Casted(CastedExpr {
                        expr: Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in(source, "1"),
                        })),
                        casted_type: Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier(source, "Exitcode"))],
                            params: Vec::new(),
                            segment: find_in(source, "Exitcode"),
                        }),
                        segment: find_in(source, "1 as Exitcode"),
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::Casted(CastedExpr {
                        expr: Box::new(Expr::Unary(UnaryOperation {
                            op: UnaryOperator::Negate,
                            expr: Box::new(Expr::Literal(Literal {
                                parsed: 1.into(),
                                segment: find_in_nth(source, "1", 1)
                            })),
                            segment: find_in(source, "-1"),
                        })),
                        casted_type: Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier(source, "Int"))],
                            params: Vec::new(),
                            segment: find_in(source, "Int"),
                        }),
                        segment: find_in(source, "-1 as Int"),
                    })),
                })),
                segment: find_between(source, "$((", "))"),
            })),
            casted_type: Type::Parametrized(ParametrizedType {
                path: vec![InclusionPathItem::Symbol(identifier(source, "Float"))],
                params: Vec::new(),
                segment: find_in(source, "Float"),
            }),
            segment: source.segment(),
        })]
    );
}

#[test]
fn lambda_in_val() {
    let source = "val x = (a) => $a + $b";
    let parsed = parse(source).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::VarDeclaration(VarDeclaration {
            kind: VarKind::Val,
            var: TypedVariable {
                name: identifier(source, "x"),
                ty: None,
            },
            initializer: Some(Box::new(Expr::LambdaDef(LambdaDef {
                args: vec![TypedVariable {
                    name: identifier_nth(source, "a", 1),
                    ty: None,
                }],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("a".into()),
                        segment: find_in(source, "$a")
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("b".into()),
                        segment: find_in(source, "$b")
                    })),
                })),
                segment: find_between(source, "(a)", "$b"),
            }))),
            segment: source.segment(),
        })]
    );
}

#[test]
fn lambda_empty_params() {
    let source = "() => $a + $b";
    let parsed = parse(source).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::LambdaDef(LambdaDef {
            args: vec![],
            body: Box::new(Expr::Binary(BinaryOperation {
                left: Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("a".into()),
                    segment: find_in(source, "$a")
                })),
                op: BinaryOperator::Plus,
                right: Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("b".into()),
                    segment: find_in(source, "$b")
                })),
            })),
            segment: source.segment(),
        })]
    );
}

#[test]
fn lambda_in_classic_call() {
    let source = "echo a => $a + $b";
    let parsed = parse(source).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source, "echo"),
                literal(source, "a"),
                literal(source, "=>"),
                Expr::VarReference(VarReference {
                    name: VarName::User("a".into()),
                    segment: find_in(source, "$a")
                }),
                literal(source, "+"),
                Expr::VarReference(VarReference {
                    name: VarName::User("b".into()),
                    segment: find_in(source, "$b")
                }),
            ]
        })]
    );
}

#[test]
fn lambda_one_arg() {
    let source = "calc(n => $n * $n)";
    let parsed = parse(source).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::ProgrammaticCall(ProgrammaticCall {
            path: vec![InclusionPathItem::Symbol(identifier(source, "calc"))],
            arguments: vec![Expr::LambdaDef(LambdaDef {
                args: vec![TypedVariable {
                    name: identifier(source, "n"),
                    ty: None,
                }],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("n".into()),
                        segment: find_in(source, "$n")
                    })),
                    op: BinaryOperator::Times,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("n".into()),
                        segment: find_in_nth(source, "$n", 1)
                    })),
                })),
                segment: find_in(source, "n => $n * $n")
            })],
            type_parameters: vec![],
            segment: source.segment(),
        })]
    );
}

#[test]
fn lambda_in_pfc() {
    let source = "calc(() => $a + $b)";
    let parsed = parse(source).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::ProgrammaticCall(ProgrammaticCall {
            path: vec![InclusionPathItem::Symbol(identifier(source, "calc"))],
            arguments: vec![Expr::LambdaDef(LambdaDef {
                args: vec![],
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("a".into()),
                        segment: find_in(source, "$a")
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("b".into()),
                        segment: find_in(source, "$b")
                    })),
                })),
                segment: find_in(source, "() => $a + $b")
            })],
            type_parameters: vec![],
            segment: source.segment(),
        })]
    );
}

#[test]
fn identity_lambda() {
    let source = "a => $a";
    let parsed = parse(source).expect("Failed to parse.");
    assert_eq!(
        parsed,
        vec![Expr::LambdaDef(LambdaDef {
            args: vec![TypedVariable {
                name: identifier(source, "a"),
                ty: None,
            }],
            body: Box::new(Expr::VarReference(VarReference {
                name: VarName::User("a".into()),
                segment: find_in(source, "$a")
            })),
            segment: source.segment(),
        })]
    );
}

#[test]
fn background_command_echo() {
    let source = "echo hello &\n";
    let parsed = parse(source).expect("Failed to parse");
    let expected = vec![Expr::Detached(Detached {
        underlying: Box::new(Expr::Call(Call {
            arguments: vec![literal(source, "echo"), literal(source, "hello")],
        })),
        segment: find_in(source, "echo hello &"),
    })];
    assert_eq!(parsed, expected);
}

#[test]
fn constructor_in_call() {
    let source = "echo Foo().test() Bar\\(\\)";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source, "echo"),
                Expr::MethodCall(MethodCall {
                    source: Box::new(Expr::ProgrammaticCall(ProgrammaticCall {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "Foo"))],
                        arguments: vec![],
                        type_parameters: vec![],
                        segment: find_in(source, "Foo()"),
                    })),
                    name: Some(identifier(source, "test")),
                    arguments: Vec::new(),
                    type_parameters: Vec::new(),
                    segment: find_in(source, ".test()")
                }),
                Expr::Literal(Literal {
                    parsed: "Bar()".into(),
                    segment: find_in(source, "Bar\\(\\)")
                }),
            ],
        })]
    );
}

#[test]
fn arithmetic_multiple_lines() {
    let source = "val n = 1\\\n + 2";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::VarDeclaration(VarDeclaration {
            kind: VarKind::Val,
            var: TypedVariable {
                name: identifier(source, "n"),
                ty: None,
            },
            initializer: Some(Box::new(Expr::Binary(BinaryOperation {
                left: Box::new(Expr::Literal(Literal {
                    parsed: 1.into(),
                    segment: find_in(source, "1"),
                })),
                op: BinaryOperator::Plus,
                right: Box::new(Expr::Literal(Literal {
                    parsed: 2.into(),
                    segment: find_in(source, "2"),
                })),
            }))),
            segment: source.segment(),
        })],
    );
}

#[test]
fn wildcard_redirect_or() {
    let source = "docker image inspect moshell:0.1 &> /dev/null || echo 'Unknown image!'";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Binary(BinaryOperation {
            left: Box::new(Expr::Redirected(Redirected {
                expr: Box::new(Expr::Call(Call {
                    arguments: vec![
                        literal(source, "docker"),
                        literal(source, "image"),
                        literal(source, "inspect"),
                        literal(source, "moshell:0.1"),
                    ],
                })),
                redirections: vec![Redir {
                    fd: RedirFd::Wildcard,
                    operator: RedirOp::Write,
                    operand: literal(source, "/dev/null"),
                    segment: find_in(source, "&> /dev/null"),
                }],
            })),
            op: BinaryOperator::Or,
            right: Box::new(Expr::Call(Call {
                arguments: vec![literal(source, "echo"), literal(source, "'Unknown image!'"),],
            })),
        })]
    );
}

#[test]
fn assign_iterable() {
    let source = "it = 1..10";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Assign(Assign {
            left: Box::new(Expr::Path(Path {
                path: vec![InclusionPathItem::Symbol(identifier(source, "it"))],
            })),
            operator: AssignOperator::Assign,
            value: Box::new(Expr::Range(Iterable::Range(NumericRange {
                start: Box::new(Expr::Literal(Literal {
                    parsed: 1.into(),
                    segment: find_in(source, "1")
                })),
                end: Box::new(Expr::Literal(Literal {
                    parsed: 10.into(),
                    segment: find_in(source, "10")
                })),
                step: None,
                upper_inclusive: false,
            }))),
        })]
    );
}

#[test]
fn for_in_step_2_range() {
    let source = "for i in 1..=10..2; break";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::For(For {
            kind: Box::new(ForKind::Range(RangeFor {
                receiver: identifier(source, "i"),
                iterable: Expr::Range(Iterable::Range(NumericRange {
                    start: Box::new(Expr::Literal(Literal {
                        parsed: 1.into(),
                        segment: find_in(source, "1")
                    })),
                    end: Box::new(Expr::Literal(Literal {
                        parsed: 10.into(),
                        segment: find_in(source, "10")
                    })),
                    step: Some(Box::new(Expr::Literal(Literal {
                        parsed: 2.into(),
                        segment: find_in(source, "2")
                    }))),
                    upper_inclusive: true,
                })),
                segment: find_in(source, "i in 1..=10..2")
            })),
            body: Box::new(Expr::Break(find_in(source, "break"))),
            segment: source.segment()
        })]
    );
}

#[test]
fn call_not_assign() {
    let source = "a '=' 5";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source, "a"),
                literal(source, "'='"),
                Expr::Literal(Literal {
                    parsed: 5.into(),
                    segment: find_in(source, "5")
                }),
            ],
        })]
    );
}

#[test]
fn constructor_assign() {
    let source = "a = !Foo(5)";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Assign(Assign {
            left: Box::new(Expr::Path(Path {
                path: vec![InclusionPathItem::Symbol(identifier(source, "a"))],
            })),
            operator: AssignOperator::Assign,
            value: Box::new(Expr::Unary(UnaryOperation {
                op: UnaryOperator::Not,
                expr: Box::new(Expr::ProgrammaticCall(ProgrammaticCall {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "Foo"))],
                    arguments: vec![Expr::Literal(Literal {
                        parsed: 5.into(),
                        segment: find_in(source, "5")
                    })],
                    type_parameters: vec![],
                    segment: find_in(source, "Foo(5)")
                })),
                segment: find_in(source, "!Foo(5)")
            })),
        })]
    );
}

#[test]
fn programmatic_call() {
    let source = "ssh('localhost', 'ls -l', 8 / 2)";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::ProgrammaticCall(ProgrammaticCall {
            path: vec![InclusionPathItem::Symbol(identifier(source, "ssh"))],
            arguments: vec![
                literal(source, "'localhost'"),
                literal(source, "'ls -l'"),
                Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Literal(Literal {
                        parsed: 8.into(),
                        segment: find_in(source, "8")
                    })),
                    op: BinaryOperator::Divide,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 2.into(),
                        segment: find_in(source, "2")
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
    let source = "ssh localhost , 'ls -l' 8 / 2";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source, "ssh"),
                literal(source, "localhost"),
                literal(source, ","),
                literal(source, "'ls -l'"),
                Expr::Literal(Literal {
                    parsed: 8.into(),
                    segment: find_in(source, "8")
                }),
                literal(source, "/"),
                Expr::Literal(Literal {
                    parsed: 2.into(),
                    segment: find_in(source, "2")
                }),
            ],
        })]
    );
}

#[test]
fn method_and_function_calls_mixed() {
    let source = "create().foo(dummy().truthy())\n.bar()";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::MethodCall(MethodCall {
            source: Box::new(Expr::MethodCall(MethodCall {
                source: Box::new(Expr::ProgrammaticCall(ProgrammaticCall {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "create"))],
                    arguments: Vec::new(),
                    type_parameters: Vec::new(),
                    segment: find_in(source, "create()")
                })),
                name: Some(identifier(source, "foo")),
                arguments: vec![Expr::MethodCall(MethodCall {
                    source: Box::new(Expr::ProgrammaticCall(ProgrammaticCall {
                        path: vec![InclusionPathItem::Symbol(identifier(source, "dummy"))],
                        arguments: Vec::new(),
                        type_parameters: Vec::new(),
                        segment: find_in(source, "dummy()")
                    })),
                    name: Some(identifier(source, "truthy")),
                    arguments: Vec::new(),
                    type_parameters: Vec::new(),
                    segment: find_in(source, ".truthy()")
                })],
                type_parameters: Vec::new(),
                segment: find_in(source, ".foo(dummy().truthy())")
            })),
            name: Some(identifier(source, "bar")),
            arguments: Vec::new(),
            type_parameters: Vec::new(),
            segment: find_in(source, ".bar()")
        })]
    );
}

#[test]
fn block_method_call() {
    let source = "{ $x }.foo('a')";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::MethodCall(MethodCall {
            source: Box::new(Expr::Block(Block {
                expressions: vec![Expr::VarReference(VarReference {
                    name: VarName::User("x".into()),
                    segment: find_in(source, "$x")
                })],
                segment: find_between(source, "{", "}")
            })),
            name: Some(identifier(source, "foo")),
            arguments: vec![literal(source, "'a'")],
            type_parameters: Vec::new(),
            segment: find_between(source, ".", ")")
        })]
    );
}

#[test]
fn method_call_with_type_params_and_ref() {
    let source = "$a.foo($d);echo \"${c.bar::[T]()}\"";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![
            Expr::MethodCall(MethodCall {
                source: Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("a".into()),
                    segment: find_in(source, "$a")
                })),
                name: Some(identifier(source, "foo")),
                arguments: vec![Expr::VarReference(VarReference {
                    name: VarName::User("d".into()),
                    segment: find_in(source, "$d")
                })],
                type_parameters: Vec::new(),
                segment: find_between(source, ".", ")")
            }),
            Expr::Call(Call {
                arguments: vec![
                    literal(source, "echo"),
                    Expr::TemplateString(TemplateString {
                        parts: vec![Expr::MethodCall(MethodCall {
                            source: Box::new(Expr::VarReference(VarReference {
                                name: VarName::User("c".into()),
                                segment: find_in(source, "${c")
                            })),
                            name: Some(identifier(source, "bar")),
                            arguments: Vec::new(),
                            type_parameters: vec![Type::Parametrized(ParametrizedType {
                                path: vec![InclusionPathItem::Symbol(identifier(source, "T"))],
                                params: Vec::new(),
                                segment: find_in(source, "T")
                            })],
                            segment: find_in(source, ".bar::[T]()")
                        })],
                        segment: find_between(source, "\"", "\"")
                    }),
                ],
            }),
        ]
    );
}

#[test]
fn comments_mix_with_spaces() {
    let source = "{
    //
    //
    ping localhost
}";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Block(Block {
            expressions: vec![Expr::Call(Call {
                arguments: vec![literal(source, "ping"), literal(source, "localhost")],
            })],
            segment: source.segment()
        })]
    );
}

#[test]
fn classic_call_no_regression() {
    let source = "test '=>' ,,here, ->..3 54a2 => 1..=9 true$a";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source, "test"),
                literal(source, "'=>'"),
                literal(source, ",,here,"),
                literal(source, "->..3"),
                literal(source, "54a2"),
                literal_nth(source, "=>", 1),
                literal(source, "1..=9"),
                Expr::TemplateString(TemplateString {
                    parts: vec![
                        literal(source, "true"),
                        Expr::VarReference(VarReference {
                            name: VarName::User("a".into()),
                            segment: find_in(source, "$a")
                        })
                    ],
                    segment: find_in(source, "true$a")
                }),
            ],
        })]
    );
}

#[test]
fn call_method_on_int() {
    let source = "1.foo(true)";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::MethodCall(MethodCall {
            source: Box::new(Expr::Literal(Literal {
                parsed: 1.into(),
                segment: find_in(source, "1")
            })),
            name: Some(identifier(source, "foo")),
            arguments: vec![Expr::Literal(Literal {
                parsed: true.into(),
                segment: find_in(source, "true")
            })],
            type_parameters: Vec::new(),
            segment: find_between(source, ".", ")")
        })]
    );
}

#[test]
fn if_branch_string() {
    let source = "if $result; 'true'; else return 'false'";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::If(If {
            condition: Box::new(Expr::VarReference(VarReference {
                name: VarName::User("result".into()),
                segment: find_in(source, "$result")
            })),
            success_branch: Box::new(literal(source, "'true'")),
            fail_branch: Some(Box::new(Expr::Return(Return {
                expr: Some(Box::new(literal(source, "'false'"))),
                segment: find_in(source, "return 'false'")
            }))),
            segment: source.segment()
        })]
    );
}

#[test]
fn precedence() {
    let source = "-$n.convert('a'..='z' as Char / 'r')";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Unary(UnaryOperation {
            op: UnaryOperator::Negate,
            expr: Box::new(Expr::MethodCall(MethodCall {
                source: Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("n".into()),
                    segment: find_in(source, "$n")
                })),
                name: Some(identifier(source, "convert")),
                arguments: vec![Expr::Range(Iterable::Range(NumericRange {
                    start: Box::new(literal(source, "'a'")),
                    end: Box::new(Expr::Binary(BinaryOperation {
                        left: Box::new(Expr::Casted(CastedExpr {
                            expr: Box::new(literal(source, "'z'")),
                            casted_type: Type::Parametrized(ParametrizedType {
                                path: vec![InclusionPathItem::Symbol(identifier(source, "Char"))],
                                params: Vec::new(),
                                segment: find_in(source, "Char")
                            }),
                            segment: find_in(source, "'z' as Char")
                        })),
                        op: BinaryOperator::Divide,
                        right: Box::new(literal(source, "'r'")),
                    })),
                    step: None,
                    upper_inclusive: true,
                }))],
                type_parameters: Vec::new(),
                segment: 3..source.len(),
            })),
            segment: source.segment(),
        })]
    );
}
