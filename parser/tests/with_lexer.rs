use lexer::lexer::lex;
use lexer::token::{Token, TokenType};
use parser::ast::callable::{Call, Pipeline, Redir, RedirFd, RedirOp};
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
            arguments: vec![
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "echo"),
                    parsed: LiteralValue::String("echo".to_string()),
                }),
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Quote, "'"),
                    parsed: LiteralValue::String("$var5".to_string()),
                }),
                Expr::VarReference(VarReference {
                    name: Token::new(TokenType::Identifier, "var5"),
                }),
            ],
            redirections: vec![],
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
            arguments: vec![
                Expr::TemplateString(vec![
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "fake"),
                        parsed: LiteralValue::String("fake".to_string()),
                    }),
                    Expr::VarReference(VarReference {
                        name: Token::new(TokenType::Identifier, "cmd"),
                    }),
                ]),
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "do"),
                    parsed: LiteralValue::String("do".to_string()),
                }),
                Expr::VarReference(VarReference {
                    name: Token::new(TokenType::Identifier, "arg2"),
                }),
            ],
            redirections: vec![],
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
            arguments: vec![
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "echo"),
                    parsed: LiteralValue::String("echo".to_string()),
                }),
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
            redirections: vec![],
        })]
    );
}

#[test]
fn with_lexer_redirection() {
    let tokens = lex("test &> /dev/null");
    let parsed = parse(tokens).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![Expr::Literal(Literal {
                token: Token::new(TokenType::Identifier, "test"),
                parsed: LiteralValue::String("test".to_string()),
            })],
            redirections: vec![Redir {
                fd: RedirFd::Wildcard,
                operator: RedirOp::Write,
                operand: Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "/dev/null"),
                    parsed: LiteralValue::String("/dev/null".to_string()),
                }),
            }],
        })]
    );
}

#[test]
fn with_lexer_pipe_and_redirection() {
    let tokens = lex("ls -l | grep 'hello' > out.txt");
    let parsed = parse(tokens).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Pipeline(Pipeline {
            commands: vec![
                Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "ls"),
                            parsed: LiteralValue::String("ls".to_string()),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "l"),
                            parsed: LiteralValue::String("-l".to_string()),
                        }),
                    ],
                    redirections: vec![],
                },
                Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "grep"),
                            parsed: LiteralValue::String("grep".to_string()),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Quote, "'"),
                            parsed: LiteralValue::String("hello".to_string()),
                        }),
                    ],
                    redirections: vec![Redir {
                        fd: RedirFd::Default,
                        operator: RedirOp::Write,
                        operand: Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "out.txt"),
                            parsed: LiteralValue::String("out.txt".to_string()),
                        }),
                    }],
                },
            ],
            negation: false,
        }),]
    );
}
