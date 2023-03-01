use lexer::token::{Token, TokenType};
use parser::ast::callable::{Call, Pipeline, Redir, RedirFd, RedirOp, Redirected};
use parser::ast::literal::Literal;
use parser::ast::substitution::{Substitution, SubstitutionKind};
use parser::ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
use parser::ast::Expr;
use parser::parse;
use parser::source::Source;
use pretty_assertions::assert_eq;

#[test]
fn with_lexer_variable() {
    let source = Source::unknown("var a = 'hello world!'");
    let parsed = parse(source).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::VarDeclaration(VarDeclaration {
            kind: VarKind::Var,
            var: TypedVariable {
                name: Token::new(TokenType::Identifier, "a"),
                ty: None,
            },
            initializer: Some(Box::new(Expr::Literal(Literal {
                lexme: "'hello world!'",
                parsed: "hello world!".into(),
            }))),
        })]
    );
}

#[test]
fn with_lexer_var_reference_one() {
    let source = Source::unknown("echo '$var5' $var5");
    let parsed = parse(source).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::Literal("echo".into()),
                Expr::Literal(Literal {
                    lexme: "'$var5'",
                    parsed: "$var5".into(),
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
    let source = Source::unknown("\"fake$cmd\" do $arg2");
    let parsed = parse(source).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::TemplateString(vec![
                    Expr::Literal("fake".into()),
                    Expr::VarReference(VarReference {
                        name: Token::new(TokenType::Identifier, "cmd"),
                    }),
                ]),
                Expr::Literal("do".into()),
                Expr::VarReference(VarReference {
                    name: Token::new(TokenType::Identifier, "arg2"),
                }),
            ],
        })]
    );
}

#[test]
fn with_lexer_var_reference_three() {
    let source = Source::unknown("echo \"hello $world everyone $verb${ready}!\"");
    let parsed = parse(source).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::Literal("echo".into()),
                Expr::TemplateString(vec![
                    Expr::Literal("hello ".into()),
                    Expr::VarReference(VarReference {
                        name: Token::new(TokenType::Identifier, "world"),
                    }),
                    Expr::Literal(" everyone ".into()),
                    Expr::VarReference(VarReference {
                        name: Token::new(TokenType::Identifier, "verb"),
                    }),
                    Expr::VarReference(VarReference {
                        name: Token::new(TokenType::Identifier, "ready"),
                    }),
                    Expr::Literal("!".into()),
                ]),
            ],
        })]
    );
}

#[test]
fn with_lexer_redirection() {
    let source = Source::unknown("test &> /dev/null");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Redirected(Redirected {
            expr: Box::new(Expr::Call(Call {
                arguments: vec![Expr::Literal("test".into())],
            })),
            redirections: vec![Redir {
                fd: RedirFd::Wildcard,
                operator: RedirOp::Write,
                operand: Expr::Literal("/dev/null".into()),
            }],
        })]
    );
}

#[test]
fn with_lexer_redirections() {
    let source = Source::unknown("command < /tmp/input 2> /tmp/output");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Redirected(Redirected {
            expr: Box::new(Expr::Call(Call {
                arguments: vec![Expr::Literal("command".into())],
            })),
            redirections: vec![
                Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::Read,
                    operand: Expr::Literal("/tmp/input".into()),
                },
                Redir {
                    fd: RedirFd::Fd(2),
                    operator: RedirOp::Write,
                    operand: Expr::Literal("/tmp/output".into()),
                },
            ],
        })]
    );
}

#[test]
fn with_lexer_pipe_and_redirection() {
    let source = Source::unknown("ls -l | grep 'hello' > out.txt");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Pipeline(Pipeline {
            commands: vec![
                Expr::Call(Call {
                    arguments: vec![Expr::Literal("ls".into()), Expr::Literal("-l".into()),],
                }),
                Expr::Redirected(Redirected {
                    expr: Box::new(Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal("grep".into()),
                            Expr::Literal(Literal {
                                lexme: "'hello'",
                                parsed: "hello".into()
                            }),
                        ]
                    })),
                    redirections: vec![Redir {
                        fd: RedirFd::Default,
                        operator: RedirOp::Write,
                        operand: Expr::Literal("out.txt".into()),
                    }],
                }),
            ],
        }),]
    );
}

#[test]
fn with_lexer_pipe_and_pipe() {
    let source = Source::unknown("ls|wc|tr -s ' '");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Pipeline(Pipeline {
            commands: vec![
                Expr::Call(Call {
                    arguments: vec![Expr::Literal("ls".into())],
                }),
                Expr::Call(Call {
                    arguments: vec![Expr::Literal("wc".into())],
                }),
                Expr::Call(Call {
                    arguments: vec![
                        Expr::Literal("tr".into()),
                        Expr::Literal("-s".into()),
                        Expr::Literal(Literal {
                            lexme: "' '",
                            parsed: " ".into(),
                        }),
                    ],
                }),
            ],
        }),]
    );
}

#[test]
fn with_lexer_here_string() {
    let source = Source::unknown("grep e <<< 'hello'");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Redirected(Redirected {
            expr: Box::new(Expr::Call(Call {
                arguments: vec![Expr::Literal("grep".into()), Expr::Literal("e".into()),]
            })),
            redirections: vec![Redir {
                fd: RedirFd::Default,
                operator: RedirOp::String,
                operand: Expr::Literal(Literal {
                    lexme: "'hello'",
                    parsed: "hello".into(),
                }),
            }],
        })]
    );
}

#[test]
fn with_lexer_substitution() {
    let source = Source::unknown("echo $(ls -l)");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::Literal("echo".into()),
                Expr::Substitution(Substitution {
                    expr: Box::new(Expr::Call(Call {
                        arguments: vec![Expr::Literal("ls".into()), Expr::Literal("-l".into()),],
                    })),
                    kind: SubstitutionKind::Capture,
                }),
            ],
        })]
    );
}

#[test]
fn with_lexer_substitution_in_substitution() {
    let source = Source::unknown("echo $( ls \"$(pwd)/test\" )");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::Literal("echo".into()),
                Expr::Substitution(Substitution {
                    expr: Box::new(Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal("ls".into()),
                            Expr::TemplateString(vec![
                                Expr::Substitution(Substitution {
                                    expr: Box::new(Expr::Call(Call {
                                        arguments: vec![Expr::Literal("pwd".into())],
                                    })),
                                    kind: SubstitutionKind::Capture,
                                }),
                                Expr::Literal("/test".into()),
                            ]),
                        ],
                    })),
                    kind: SubstitutionKind::Capture,
                }),
            ],
        })]
    );
}

#[test]
fn with_lexer_here_invoke() {
    let source = Source::unknown("val valid = @(nginx -t)");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::VarDeclaration(VarDeclaration {
            kind: VarKind::Val,
            var: TypedVariable {
                name: Token::new(TokenType::Identifier, "valid"),
                ty: None,
            },
            initializer: Some(Box::new(Expr::Substitution(Substitution {
                expr: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("nginx".into()), Expr::Literal("-t".into()),],
                })),
                kind: SubstitutionKind::Return,
            }))),
        })]
    );
}
