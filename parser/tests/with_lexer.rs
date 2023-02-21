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
fn with_lexer_var_reference_one() {
    let tokens = lex("echo '$var5' $var5");
    let parsed = parse(tokens).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            command: Box::new(Expr::Literal(Literal {
                token: Token::new(TokenType::Identifier, "echo"),
                parsed: LiteralValue::String("echo".to_string()),
            })),
            arguments: vec![
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Quote, "'"),
                    parsed: LiteralValue::String("$var5".to_string()),
                }),
                Expr::VarReference(VarReference {
                    name: Token::new(TokenType::Identifier, "var5"),
                }),
            ],
        })]
    );
}

#[test]
fn with_lexer_var_reference_two() {
    let tokens = lex("\"fake$cmd\" do $arg2");
    let parsed = parse(tokens).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            command: Box::new(Expr::TemplateString(vec![
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "fake"),
                    parsed: LiteralValue::String("fake".to_string()),
                }),
                Expr::VarReference(VarReference {
                    name: Token::new(TokenType::Identifier, "cmd"),
                }),
            ])),
            arguments: vec![
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

#[test]
fn with_lexer_var_reference_three() {
    let tokens = lex("echo \"hello $world everyone $verb${ready}!\"");
    let parsed = parse(tokens).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            command: Box::new(Expr::Literal(Literal {
                token: Token::new(TokenType::Identifier, "echo"),
                parsed: LiteralValue::String("echo".to_string()),
            })),
            arguments: vec![
                Expr::TemplateString(vec![
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "hello"),
                        parsed: LiteralValue::String("hello ".to_string()),
                    }),
                    Expr::VarReference(VarReference {
                        name: Token::new(TokenType::Identifier, "world"),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Space, " "),
                        parsed: LiteralValue::String(" everyone ".to_string()),
                    }),
                    Expr::VarReference(VarReference {
                        name: Token::new(TokenType::Identifier, "verb"),
                    }),
                    Expr::VarReference(VarReference {
                        name: Token::new(TokenType::Identifier, "ready"),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Not, "!"),
                        parsed: LiteralValue::String("!".to_string()),
                    }),
                ]),
            ],
        })]
    );
}
