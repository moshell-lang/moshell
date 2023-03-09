use context::source::Source;
use parser::ast::callable::{Call, Redir, RedirFd, RedirOp, Redirected};
use parser::ast::operation::{BinaryOperation, BinaryOperator};
use parser::ast::structure::Construct;
use parser::ast::value::{Literal, LiteralValue};
use parser::ast::variable::{TypedVariable, VarDeclaration, VarKind};
use parser::ast::Expr;
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
            ty: Some("int"),
        },
        initializer: Some(Box::new(Expr::Literal(Literal {
            lexeme: "1",
            parsed: LiteralValue::Int(1),
        }))),
    })];
    assert_eq!(parsed, expected);
}

#[test]
fn command_echo() {
    let source = Source::unknown("echo hello");
    let parsed = parse(source).expect("Failed to parse");

    let expected = vec![Expr::Call(Call {
        arguments: vec![Expr::Literal("echo".into()), Expr::Literal("hello".into())],
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
                Expr::Construct(Construct {
                    name: "Foo",
                    args: vec![],
                }),
                Expr::Literal(Literal {
                    lexeme: "Bar\\(\\)",
                    parsed: "Bar()".into(),
                }),
            ],
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
            })),
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
        })]
    );
}
