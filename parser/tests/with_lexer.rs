use context::source::Source;
use parser::ast::callable::{Call, Pipeline, Redir, RedirFd, RedirOp, Redirected};
use parser::ast::group::Subshell;
use parser::ast::literal::Literal;
use parser::ast::substitution::{Substitution, SubstitutionKind};
use parser::ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
use parser::ast::Expr;
use parser::err::{ParseError, ParseErrorKind, ParseReport};
use parser::parse;
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
    let source = Source::unknown("echo '$var5' $var5");
    let parsed = parse(source).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::Literal("echo".into()),
                Expr::Literal(Literal {
                    lexeme: "'$var5'",
                    parsed: "$var5".into(),
                }),
                Expr::VarReference(VarReference { name: "var5" }),
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
                    Expr::VarReference(VarReference { name: "cmd" }),
                ]),
                Expr::Literal("do".into()),
                Expr::VarReference(VarReference { name: "arg2" }),
            ],
        })]
    );
}

#[test]
fn empty_content() {
    let source = Source::unknown("\n\n//empty lines\n\n");
    let result = parse(source).expect("Failed to parse");
    assert_eq!(result, vec![])
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
                    Expr::VarReference(VarReference { name: "world" }),
                    Expr::Literal(" everyone ".into()),
                    Expr::VarReference(VarReference { name: "verb" }),
                    Expr::VarReference(VarReference { name: "ready" }),
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
                    lexeme: "'hello'",
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
    let source = Source::unknown("echo $( ls \"$(pwd)/test\" )");
    let parsed = parse(source).expect("Failed to parse");
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

#[test]
fn repos_delimiter_stack() {
    let content = "{\n\
    val n=$(test $n})\n\
    echo invalid ]\n\
    }\n\
    echo end\n\
    val n=9!3";
    let source = Source::unknown(content);
    let report = parse(source);
    assert_eq!(
        report,
        ParseReport {
            expr: vec![Expr::Call(Call {
                arguments: vec![Expr::Literal("echo".into()), Expr::Literal("end".into())],
            })],
            errors: vec![
                ParseError {
                    message: "Mismatched closing delimiter.".to_string(),
                    position: content.find('}').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unpaired(content.find('(').map(|p| p..p + 1).unwrap())
                },
                ParseError {
                    message: "invalid infix operator".to_string(),
                    position: content.rfind('!').map(|p| p..p + 1).unwrap(),
                    kind: ParseErrorKind::Unexpected
                }
            ],
        }
    );
}
