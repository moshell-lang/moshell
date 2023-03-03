use lexer::lexer::lex;
use parser::ast::callable::{Call, Pipeline, Redir, RedirFd, RedirOp, Redirected};
use parser::ast::group::Subshell;
use parser::ast::literal::Literal;
use parser::ast::substitution::{Substitution, SubstitutionKind};
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
                name: "a",
                ty: None,
            },
            initializer: Some(Box::new(Expr::Literal(Literal {
                lexeme: "'hello world!'",
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
                Expr::Literal("echo".into()),
                Expr::Literal(Literal {
                    lexeme: "'$var5'",
                    parsed: "$var5".into(),
                }),
                Expr::VarReference(VarReference {
                    name: "var5",
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
            arguments: vec![
                Expr::TemplateString(vec![
                    Expr::Literal("fake".into()),
                    Expr::VarReference(VarReference {
                        name: "cmd",
                    }),
                ]),
                Expr::Literal("do".into()),
                Expr::VarReference(VarReference {
                    name: "arg2",
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
            arguments: vec![
                Expr::Literal("echo".into()),
                Expr::TemplateString(vec![
                    Expr::Literal("hello ".into()),
                    Expr::VarReference(VarReference {
                        name: "world",
                    }),
                    Expr::Literal(" everyone ".into()),
                    Expr::VarReference(VarReference {
                        name: "verb",
                    }),
                    Expr::VarReference(VarReference {
                        name: "ready",
                    }),
                    Expr::Literal("!".into()),
                ]),
            ],
        })]
    );
}

#[test]
fn with_lexer_redirection() {
    let tokens = lex("test &> /dev/null");
    let parsed = parse(tokens).expect("Failed to parse");
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
    let tokens = lex("command < /tmp/input 2> /tmp/output");
    let parsed = parse(tokens).expect("Failed to parse");
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
    let tokens = lex("ls -l | grep 'hello' > out.txt");
    let parsed = parse(tokens).expect("Failed to parse");
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
                                lexeme: "'hello'",
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
    let tokens = lex("ls|wc|tr -s ' '");
    let parsed = parse(tokens).expect("Failed to parse");
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
                            lexeme: "' '",
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
    let tokens = lex("grep e <<< 'hello'");
    let parsed = parse(tokens).expect("Failed to parse");
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
                    lexeme: "'hello'",
                    parsed: "hello".into(),
                }),
            }],
        })]
    );
}

#[test]
fn with_lexer_substitution() {
    let tokens = lex("echo $(ls -l)");
    let parsed = parse(tokens).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::Literal("echo".into()),
                Expr::Substitution(Substitution {
                    underlying: Subshell {
                        expressions: vec![Expr::Call(Call {
                            arguments: vec![Expr::Literal("ls".into()), Expr::Literal("-l".into())],
                        })]
                    },
                    kind: SubstitutionKind::Capture,
                }),
            ],
        })]
    );
}

#[test]
fn with_lexer_substitution_in_substitution() {
    let tokens = lex("echo $( ls \"$(pwd)/test\" )");
    let parsed = parse(tokens).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::Literal("echo".into()),
                Expr::Substitution(Substitution {
                    underlying: Subshell {
                        expressions: vec![Expr::Call(Call {
                            arguments: vec![
                                Expr::Literal("ls".into()),
                                Expr::TemplateString(vec![
                                    Expr::Substitution(Substitution {
                                        underlying: Subshell {
                                            expressions: vec![Expr::Call(Call {
                                                arguments: vec![Expr::Literal("pwd".into())]
                                            })],
                                        },
                                        kind: SubstitutionKind::Capture,
                                    }),
                                    Expr::Literal("/test".into()),
                                ]),
                            ],
                        })]
                    },
                    kind: SubstitutionKind::Capture,
                }),
            ],
        })]
    );
}
