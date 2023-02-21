use lexer::token::{Token, TokenType};
use parser::ast::callable::Call;
use parser::ast::literal::{Literal, LiteralValue};
use parser::ast::variable::{TypedVariable, VarDeclaration, VarKind};
use parser::ast::Expr;
use parser::parse;

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
            name: Token::new(TokenType::Identifier, "a"),
            ty: Some(Token::new(TokenType::Identifier, "int")),
        },
        initializer: Some(Box::new(Expr::Literal(Literal {
            token: Token::new(TokenType::IntLiteral, "1"),
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
        Token::new(TokenType::Quote, "'"),
        Token::new(TokenType::Identifier, "hello"),
        Token::new(TokenType::Quote, "'"),
    ];
    let parsed = parse(tokens).expect("Failed to parse");

    let expected = vec![Expr::Call(Call {
        command: Box::new(Expr::Literal(Literal {
            token: Token::new(TokenType::Identifier, "echo"),
            parsed: LiteralValue::String("echo".to_string()),
        })),

        arguments: vec![
            Expr::Literal(Literal {
                token: Token::new(TokenType::Quote, "'"),
                parsed: LiteralValue::String("hello".to_string()),
            }),
        ],
    })];
    assert_eq!(parsed, expected);
}
