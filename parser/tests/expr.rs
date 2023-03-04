use lexer::token::{Token, TokenType};
use parser::ast::callable::Call;
use parser::ast::literal::{Literal, LiteralValue};
use parser::ast::variable::{TypedVariable, VarDeclaration, VarKind};
use parser::ast::Expr;
use parser::parse;
use pretty_assertions::assert_eq;

#[test]
fn variable_type_and_initializer() {
    let tokens = vec![
        Token::new(TokenType::Var, "var"),
        Token::new(TokenType::Space, " "),
        Token::new(TokenType::Identifier, "a"),
        Token::new(TokenType::Colon, ":"),
        Token::new(TokenType::Identifier, "int"),
        Token::new(TokenType::Equal, "="),
        Token::new(TokenType::IntLiteral, "1"),
    ];
    let parsed = parse(tokens).expect("Failed to parse");

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
    let tokens = vec![
        Token::new(TokenType::Identifier, "echo"),
        Token::new(TokenType::Space, " "),
        Token::new(TokenType::Identifier, "hello"),
    ];
    let parsed = parse(tokens).expect("Failed to parse");

    let expected = vec![Expr::Call(Call {
        arguments: vec![Expr::Literal("echo".into()), Expr::Literal("hello".into())],
    })];
    assert_eq!(parsed, expected);
}
