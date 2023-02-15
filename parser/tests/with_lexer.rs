use lexer::lexer::lex;
use lexer::token::{Token, TokenType};
use parser::ast::callable::Call;
use parser::ast::literal::{Literal, LiteralValue};
use parser::ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
use parser::ast::Expr;
use parser::parse;

#[test]
fn with_lexer_variable() {
    let tokens = lex("var a = 'hello world!'");
    let parsed = parse(tokens).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::VarDeclaration(VarDeclaration {
            kind: VarKind::Var,
            var: TypedVariable {
                name: Token::new(TokenType::Identifier, "a"),
                ty: None,
            },
            initializer: Some(Box::new(Expr::Literal(Literal {
                token: Token::new(TokenType::Quote, "'"),
                parsed: LiteralValue::String("hello world!".to_string()),
            }))),
        })]
    );
}

#[test]
fn with_lexer_var_call() {
    let tokens = lex("val r = echo 5");
    let parsed = parse(tokens).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::VarDeclaration(VarDeclaration {
            kind: VarKind::Val,
            var: TypedVariable {
                name: Token::new(TokenType::Identifier, "r"),
                ty: None,
            },
            initializer: Some(Box::new(Expr::Call(Call {
                arguments: vec![
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "echo"),
                        parsed: LiteralValue::String("echo".to_string()),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::IntLiteral, "5"),
                        parsed: LiteralValue::Int(5),
                    })
                ],
            }))),
        })]
    );
}

#[test]
fn with_lexer_var_reference() {
    let tokens = lex("fake $cmd do $arg2");
    let parsed = parse(tokens).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "fake"),
                    parsed: LiteralValue::String("fake".to_string()),
                }),
                Expr::VarReference(VarReference {
                    name: Token::new(TokenType::Identifier, "cmd"),
                }),
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "do"),
                    parsed: LiteralValue::String("do".to_string()),
                }),
                Expr::VarReference(VarReference {
                    name: Token::new(TokenType::Identifier, "arg2"),
                }),
            ],
        })]
    );
}
