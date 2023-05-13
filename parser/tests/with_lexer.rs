use ast::call::{Call, Pipeline, Redir, RedirFd, RedirOp, Redirected};
use ast::control_flow::While;
use ast::group::{Block, Subshell};
use ast::substitution::{Substitution, SubstitutionKind};
use ast::value::TemplateString;
use ast::variable::{TypedVariable, VarDeclaration, VarKind, VarReference};
use ast::Expr;
use context::source::{Source, SourceSegmentHolder};
use context::str_find::{find_in, find_in_nth};
use parser::parse;
use parser::source::{literal, literal_nth};
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
                segment: find_in_nth(source.source, "a", 1),
            },
            initializer: Some(Box::new(literal(source.source, "'hello world!'"))),
            segment: source.segment(),
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
            path: Vec::new(),
            arguments: vec![
                literal(source.source, "echo"),
                literal(source.source, "'$var5'"),
                Expr::VarReference(VarReference {
                    name: "var5",
                    segment: find_in_nth(source.source, "$var5", 1),
                }),
            ],
            type_parameters: Vec::new()
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
            path: Vec::new(),
            arguments: vec![
                Expr::TemplateString(TemplateString {
                    parts: vec![
                        literal(source.source, "\"fake"),
                        Expr::VarReference(VarReference {
                            name: "cmd",
                            segment: find_in(source.source, "$cmd"),
                        }),
                    ]
                }),
                literal(source.source, "do"),
                Expr::VarReference(VarReference {
                    name: "arg2",
                    segment: find_in(source.source, "$arg2"),
                }),
            ],
            type_parameters: Vec::new()
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
            path: Vec::new(),
            arguments: vec![
                literal(source.source, "echo"),
                Expr::TemplateString(TemplateString {
                    parts: vec![
                        literal(source.source, "\"hello "),
                        Expr::VarReference(VarReference {
                            name: "world",
                            segment: find_in(source.source, "$world"),
                        }),
                        literal(source.source, " everyone "),
                        Expr::VarReference(VarReference {
                            name: "verb",
                            segment: find_in(source.source, "$verb"),
                        }),
                        Expr::VarReference(VarReference {
                            name: "ready",
                            segment: find_in(source.source, "${ready}"),
                        }),
                        literal(source.source, "!\""),
                    ]
                }),
            ],
            type_parameters: Vec::new()
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
                path: Vec::new(),
                arguments: vec![literal(source.source, "test")],
                type_parameters: Vec::new(),
            })),
            redirections: vec![Redir {
                fd: RedirFd::Wildcard,
                operator: RedirOp::Write,
                operand: literal(source.source, "/dev/null"),
                segment: find_in(source.source, "&> /dev/null"),
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
                path: Vec::new(),
                arguments: vec![literal(source.source, "command")],
                type_parameters: Vec::new(),
            })),
            redirections: vec![
                Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::Read,
                    operand: literal(source.source, "/tmp/input"),
                    segment: find_in(source.source, "< /tmp/input"),
                },
                Redir {
                    fd: RedirFd::Fd(2),
                    operator: RedirOp::Write,
                    operand: literal(source.source, "/tmp/output"),
                    segment: find_in(source.source, "2> /tmp/output"),
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
                    path: Vec::new(),
                    arguments: vec![literal(source.source, "ls"), literal(source.source, "-l")],
                    type_parameters: Vec::new(),
                }),
                Expr::Redirected(Redirected {
                    expr: Box::new(Expr::Call(Call {
                        path: Vec::new(),
                        arguments: vec![
                            literal(source.source, "grep"),
                            literal(source.source, "'hello'"),
                        ],
                        type_parameters: Vec::new()
                    })),
                    redirections: vec![Redir {
                        fd: RedirFd::Default,
                        operator: RedirOp::Write,
                        operand: literal(source.source, "out.txt"),
                        segment: find_in(source.source, "> out.txt"),
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
                    path: Vec::new(),
                    arguments: vec![literal(source.source, "ls")],
                    type_parameters: Vec::new(),
                }),
                Expr::Call(Call {
                    path: Vec::new(),
                    arguments: vec![literal(source.source, "wc")],
                    type_parameters: Vec::new(),
                }),
                Expr::Call(Call {
                    path: Vec::new(),
                    arguments: vec![
                        literal(source.source, "tr"),
                        literal(source.source, "-s"),
                        literal(source.source, "' '"),
                    ],
                    type_parameters: Vec::new()
                }),
            ],
        }),]
    );
}

#[test]
fn with_lexer_here_string() {
    let content = "grep e <<< 'hello'";
    let source = Source::unknown("grep e <<< 'hello'");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Redirected(Redirected {
            expr: Box::new(Expr::Call(Call {
                path: Vec::new(),
                arguments: vec![literal(content, "grep"), literal_nth(content, "e", 1)],
                type_parameters: Vec::new(),
            })),
            redirections: vec![Redir {
                fd: RedirFd::Default,
                operator: RedirOp::String,
                operand: literal(source.source, "'hello'"),
                segment: find_in(content, "<<< 'hello'"),
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
            path: Vec::new(),
            arguments: vec![
                literal(source.source, "echo"),
                Expr::Substitution(Substitution {
                    underlying: Subshell {
                        expressions: vec![Expr::Call(Call {
                            path: Vec::new(),
                            arguments: vec![
                                literal(source.source, "ls"),
                                literal(source.source, "-l"),
                            ],
                            type_parameters: Vec::new(),
                        })],
                        segment: find_in(source.source, "$(ls -l)"),
                    },
                    kind: SubstitutionKind::Capture,
                }),
            ],
            type_parameters: Vec::new()
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
            path: Vec::new(),
            arguments: vec![
                literal(source.source, "echo"),
                Expr::Substitution(Substitution {
                    underlying: Subshell {
                        expressions: vec![Expr::Call(Call {
                            path: Vec::new(),
                            arguments: vec![
                                literal(source.source, "ls"),
                                Expr::TemplateString(TemplateString {
                                    parts: vec![
                                        Expr::Substitution(Substitution {
                                            underlying: Subshell {
                                                expressions: vec![Expr::Call(Call {
                                                    path: Vec::new(),
                                                    arguments: vec![literal(source.source, "pwd")],
                                                    type_parameters: Vec::new(),
                                                })],
                                                segment: find_in(source.source, "$(pwd)"),
                                            },
                                            kind: SubstitutionKind::Capture,
                                        }),
                                        literal(source.source, "/test\""),
                                    ]
                                }),
                            ],
                            type_parameters: Vec::new(),
                        })],
                        segment: find_in(source.source, "$( ls \"$(pwd)/test\" )"),
                    },
                    kind: SubstitutionKind::Capture,
                }),
            ],
            type_parameters: Vec::new()
        })]
    );
}

#[test]
fn pipe_expressions() {
    let source = Source::unknown("find . | while read -r filename { echo $filename }");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Pipeline(Pipeline {
            commands: vec![
                Expr::Call(Call {
                    path: Vec::new(),
                    arguments: vec![literal(source.source, "find"), literal(source.source, "."),],
                    type_parameters: Vec::new(),
                }),
                Expr::While(While {
                    condition: Box::new(Expr::Call(Call {
                        path: Vec::new(),
                        arguments: vec![
                            literal(source.source, "read"),
                            literal(source.source, "-r"),
                            literal(source.source, "filename"),
                        ],
                        type_parameters: Vec::new(),
                    })),
                    body: Box::new(Expr::Block(Block {
                        expressions: vec![Expr::Call(Call {
                            path: Vec::new(),
                            arguments: vec![
                                literal(source.source, "echo"),
                                Expr::VarReference(VarReference {
                                    name: "filename",
                                    segment: find_in(source.source, "$filename"),
                                }),
                            ],
                            type_parameters: Vec::new(),
                        })],
                        segment: find_in(source.source, "{ echo $filename }"),
                    })),
                    segment: find_in(source.source, "while read -r filename { echo $filename }"),
                })
            ],
        })]
    );
}
