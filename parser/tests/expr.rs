use context::source::Source;
use parser::ast::callable::Call;
use parser::ast::value::{Literal, LiteralValue};
use parser::ast::variable::{TypedVariable, VarDeclaration, VarKind};
use parser::ast::Expr;
use parser::parse;
use pretty_assertions::assert_eq;
use parser::ast::operation::BinaryOperation;
use parser::ast::operation::BinaryOperator::Plus;

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
                op: Plus,
                right: Box::new(Expr::Literal(Literal {
                    lexeme: "2",
                    parsed: 2.into(),
                })),
            }))),
        })],
    );
}