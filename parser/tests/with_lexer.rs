use lexer::lexer::lex;
use lexer::token::{Token, TokenType};
use parser::ast::callable::{Call, Pipeline, Redir, RedirFd, RedirOp};
use parser::ast::literal::{Literal, LiteralValue};
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
                    token: Token::new(TokenType::Identifier, "null"),
                    parsed: LiteralValue::String("/dev/null".to_string()),
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
                parsed: LiteralValue::String("command".to_string()),
            })],
            redirections: vec![
                Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::Read,
                    operand: Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "input"),
                        parsed: LiteralValue::String("/tmp/input".to_string()),
                    }),
                },
                Redir {
                    fd: RedirFd::Fd(2),
                    operator: RedirOp::Write,
                    operand: Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "output"),
                        parsed: LiteralValue::String("/tmp/output".to_string()),
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
                            token: Token::new(TokenType::Identifier, "txt"),
                            parsed: LiteralValue::String("out.txt".to_string()),
                        }),
                    }],
                },
            ],
            negation: false,
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
                        parsed: LiteralValue::String("ls".to_string()),
                    })],
                    redirections: vec![],
                },
                Call {
                    arguments: vec![Expr::Literal(Literal {
                        token: Token::new(TokenType::Identifier, "wc"),
                        parsed: LiteralValue::String("wc".to_string()),
                    })],
                    redirections: vec![],
                },
                Call {
                    arguments: vec![
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "tr"),
                            parsed: LiteralValue::String("tr".to_string()),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Identifier, "s"),
                            parsed: LiteralValue::String("-s".to_string()),
                        }),
                        Expr::Literal(Literal {
                            token: Token::new(TokenType::Quote, "'"),
                            parsed: LiteralValue::String(" ".to_string()),
                        }),
                    ],
                    redirections: vec![],
                },
            ],
            negation: false,
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
                    parsed: LiteralValue::String("grep".to_string()),
                }),
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "e"),
                    parsed: LiteralValue::String("e".to_string()),
                }),
            ],
            redirections: vec![Redir {
                fd: RedirFd::Default,
                operator: RedirOp::String,
                operand: Expr::Literal(Literal {
                    token: Token::new(TokenType::Quote, "'"),
                    parsed: LiteralValue::String("hello".to_string()),
                }),
            }],
        })]
    );
}
