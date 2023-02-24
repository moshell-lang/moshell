use lexer::lexer::lex;
use lexer::token::{Token, TokenType};
use parser::ast::callable::{Call, Pipeline, Redir, RedirFd, RedirOp};
use parser::ast::literal::Literal;
use parser::ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
use parser::ast::Expr;
use parser::parse;
use pretty_assertions::assert_eq;

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
                parsed: "hello world!".into(),
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
                    parsed: "echo".into(),
                }),
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Quote, "'"),
                    parsed: "$var5".into(),
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
                        parsed: "fake".into(),
                    }),
                    Expr::VarReference(VarReference {
                        name: Token::new(TokenType::Identifier, "cmd"),
                    }),
                ]),
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "do"),
                    parsed: "do".into(),
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
                    parsed: "echo".into(),
                }),
                Expr::TemplateString(vec![
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "hello"),
                        parsed: "hello ".into(),
                    }),
                    Expr::VarReference(VarReference {
                        name: Token::new(TokenType::Identifier, "world"),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Space, " "),
                        parsed: " everyone ".into(),
                    }),
                    Expr::VarReference(VarReference {
                        name: Token::new(TokenType::Identifier, "verb"),
                    }),
                    Expr::VarReference(VarReference {
                        name: Token::new(TokenType::Identifier, "ready"),
                    }),
                    Expr::Literal(Literal {
                        token: Token::new(TokenType::Not, "!"),
                        parsed: "!".into(),
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
                parsed: "test".into(),
            })],
            redirections: vec![Redir {
                fd: RedirFd::Wildcard,
                operator: RedirOp::Write,
                operand: Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "null"),
                    parsed: "/dev/null".into(),
                }),
            }],
        })]
    );
}

#[test]
fn with_lexer_redirections() {
    let tokens = lex("command < /tmp/input 2> /tmp/output");
    let parsed = parse(tokens).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![Expr::Literal(Literal {
                token: Token::new(TokenType::Identifier, "command"),
                parsed: "command".into(),
            })],
            redirections: vec![
                Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::Read,
                    operand: Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "input"),
                        parsed: "/tmp/input".into(),
                    }),
                },
                Redir {
                    fd: RedirFd::Fd(2),
                    operator: RedirOp::Write,
                    operand: Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "output"),
                        parsed: "/tmp/output".into(),
                    }),
                },
            ],
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
                            parsed: "ls".into(),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "l"),
                            parsed: "-l".into()
                        }),
                    ],
                    redirections: vec![],
                },
                Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "grep"),
                            parsed: "grep".into()
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Quote, "'"),
                            parsed: "hello".into()
                        }),
                    ],
                    redirections: vec![Redir {
                        fd: RedirFd::Default,
                        operator: RedirOp::Write,
                        operand: Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "txt"),
                            parsed: "out.txt".into(),
                        }),
                    }],
                },
            ],
        }),]
    );
}

#[test]
fn with_lexer_pipe_and_pipe() {
    let tokens = lex("ls|wc|tr -s ' '");
    let parsed = parse(tokens).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Pipeline(Pipeline {
            commands: vec![
                Call {
                    arguments: vec![Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "ls"),
                        parsed: "ls".into(),
                    })],
                    redirections: vec![],
                },
                Call {
                    arguments: vec![Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "wc"),
                        parsed: "wc".into(),
                    })],
                    redirections: vec![],
                },
                Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "tr"),
                            parsed: "tr".into(),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "s"),
                            parsed: "-s".into(),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Quote, "'"),
                            parsed: " ".into(),
                        }),
                    ],
                    redirections: vec![],
                },
            ],
        }),]
    );
}

#[test]
fn with_lexer_here_string() {
    let tokens = lex("grep e <<< 'hello'");
    let parsed = parse(tokens).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "grep"),
                    parsed: "grep".into(),
                }),
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "e"),
                    parsed: "e".into(),
                }),
            ],
            redirections: vec![Redir {
                fd: RedirFd::Default,
                operator: RedirOp::String,
                operand: Expr::Literal(Literal {
                    token: Token::new(TokenType::Quote, "'"),
                    parsed: "hello".into(),
                }),
            }],
        })]
    );
}
