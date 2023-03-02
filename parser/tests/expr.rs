use context::source::Source;
use parser::ast::callable::Call;
use parser::ast::literal::{Literal, LiteralValue};
use parser::ast::variable::{TypedVariable, VarDeclaration, VarKind};
use parser::ast::Expr;
use parser::parse;
use pretty_assertions::assert_eq;

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
            lexme: "1",
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
