use pretty_assertions::assert_eq;

use ast::call::{
    Call, MethodCall, Pipeline, ProgrammaticCall, Redir, RedirFd, RedirOp, Redirected,
};
use ast::control_flow::{Loop, While};
use ast::function::Return;
use ast::group::{Block, Subshell};
use ast::operation::{BinaryOperation, BinaryOperator};
use ast::r#use::InclusionPathItem;
use ast::range::{FilePattern, Iterable, Subscript};
use ast::substitution::{Substitution, SubstitutionKind};
use ast::value::{Literal, TemplateString};
use ast::variable::{
    Assign, AssignOperator, Path, Tilde, TildeExpansion, TypedVariable, VarDeclaration, VarKind,
    VarName, VarReference,
};
use ast::Expr;
use context::source::SourceSegmentHolder;
use context::str_find::{find_between, find_in, find_in_nth};
use parser::err::{ParseError, ParseErrorKind};
use parser::parse;
use parser::source::{identifier, identifier_nth, literal, literal_nth};

#[test]
fn backslash() {
    let source = r"\";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(parsed, Vec::new());
}

#[test]
fn with_lexer_variable() {
    let source = "var a = 'hello world!'";
    let parsed = parse(source).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::VarDeclaration(VarDeclaration {
            kind: VarKind::Var,
            var: TypedVariable {
                name: identifier_nth(source, "a", 1),
                ty: None,
            },
            initializer: Some(Box::new(literal(source, "'hello world!'"))),
            segment: source.segment(),
        })]
    );
}

#[test]
fn with_lexer_var_reference_one() {
    let source = "echo '$var5' $var5";
    let parsed = parse(source).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source, "echo"),
                literal(source, "'$var5'"),
                Expr::VarReference(VarReference {
                    name: VarName::User("var5".into()),
                    segment: find_in_nth(source, "$var5", 1),
                }),
            ],
        })]
    );
}

#[test]
fn with_lexer_var_reference_two() {
    let source = "`fake$cmd` do $arg2";
    let parsed = parse(source).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::TemplateString(TemplateString {
                    parts: vec![
                        literal(source, "fake"),
                        Expr::VarReference(VarReference {
                            name: VarName::User("cmd".into()),
                            segment: find_in(source, "$cmd"),
                        }),
                    ],
                    segment: find_in(source, "`fake$cmd`"),
                }),
                literal(source, "do"),
                Expr::VarReference(VarReference {
                    name: VarName::User("arg2".into()),
                    segment: find_in(source, "$arg2"),
                }),
            ],
        })]
    );
}

#[test]
fn empty_source() {
    let source = "\n\n//empty lines\n\n";
    let result = parse(source).expect("Failed to parse");
    assert_eq!(result, vec![])
}

#[test]
fn with_lexer_var_reference_three() {
    let source = "echo \"hello $world everyone $verb${ready}!\"";
    let parsed = parse(source).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source, "echo"),
                Expr::TemplateString(TemplateString {
                    parts: vec![
                        literal(source, "hello "),
                        Expr::VarReference(VarReference {
                            name: VarName::User("world".into()),
                            segment: find_in(source, "$world"),
                        }),
                        literal(source, " everyone "),
                        Expr::VarReference(VarReference {
                            name: VarName::User("verb".into()),
                            segment: find_in(source, "$verb"),
                        }),
                        Expr::VarReference(VarReference {
                            name: VarName::User("ready".into()),
                            segment: find_in(source, "${ready}"),
                        }),
                        literal(source, "!"),
                    ],
                    segment: find_between(source, "\"", "\""),
                }),
            ],
        })]
    );
}

#[test]
fn with_lexer_redirection() {
    let source = "test &> /dev/null";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Redirected(Redirected {
            expr: Box::new(Expr::Call(Call {
                arguments: vec![literal(source, "test")],
            })),
            redirections: vec![Redir {
                fd: RedirFd::Wildcard,
                operator: RedirOp::Write,
                operand: literal(source, "/dev/null"),
                segment: find_in(source, "&> /dev/null"),
            }],
        })]
    );
}

#[test]
fn with_lexer_redirections() {
    let source = "command < /tmp/input 2> /tmp/output";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Redirected(Redirected {
            expr: Box::new(Expr::Call(Call {
                arguments: vec![literal(source, "command")],
            })),
            redirections: vec![
                Redir {
                    fd: RedirFd::Default,
                    operator: RedirOp::Read,
                    operand: literal(source, "/tmp/input"),
                    segment: find_in(source, "< /tmp/input"),
                },
                Redir {
                    fd: RedirFd::Fd(2),
                    operator: RedirOp::Write,
                    operand: literal(source, "/tmp/output"),
                    segment: find_in(source, "2> /tmp/output"),
                },
            ],
        })]
    );
}

#[test]
fn with_lexer_pipe_and_redirection() {
    let source = "ls -l | grep 'hello' > out.txt";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Pipeline(Pipeline {
            commands: vec![
                Expr::Call(Call {
                    arguments: vec![literal(source, "ls"), literal(source, "-l")],
                }),
                Expr::Redirected(Redirected {
                    expr: Box::new(Expr::Call(Call {
                        arguments: vec![literal(source, "grep"), literal(source, "'hello'"),],
                    })),
                    redirections: vec![Redir {
                        fd: RedirFd::Default,
                        operator: RedirOp::Write,
                        operand: literal(source, "out.txt"),
                        segment: find_in(source, "> out.txt"),
                    }],
                }),
            ],
        }),]
    );
}

#[test]
fn with_lexer_pipe_and_pipe() {
    let source = "ls|wc|tr -s ' '";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Pipeline(Pipeline {
            commands: vec![
                Expr::Call(Call {
                    arguments: vec![literal(source, "ls")],
                }),
                Expr::Call(Call {
                    arguments: vec![literal(source, "wc")],
                }),
                Expr::Call(Call {
                    arguments: vec![
                        literal(source, "tr"),
                        literal(source, "-s"),
                        literal(source, "' '"),
                    ],
                }),
            ],
        }),]
    );
}

#[test]
fn with_lexer_here_string() {
    let source = "grep e <<< 'hello'";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Redirected(Redirected {
            expr: Box::new(Expr::Call(Call {
                arguments: vec![literal(source, "grep"), literal_nth(source, "e", 1)],
            })),
            redirections: vec![Redir {
                fd: RedirFd::Default,
                operator: RedirOp::String,
                operand: literal(source, "'hello'"),
                segment: find_in(source, "<<< 'hello'"),
            }],
        })]
    );
}

#[test]
fn with_lexer_substitution() {
    let source = "echo $(ls -l)";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source, "echo"),
                Expr::Substitution(Substitution {
                    underlying: Subshell {
                        expressions: vec![Expr::Call(Call {
                            arguments: vec![literal(source, "ls"), literal(source, "-l"),],
                        })],
                        segment: find_in(source, "$(ls -l)"),
                    },
                    kind: SubstitutionKind::Capture,
                }),
            ],
        })]
    );
}

#[test]
fn with_lexer_substitution_in_substitution() {
    let source = "echo $( ls \"$(pwd)/test\" )";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source, "echo"),
                Expr::Substitution(Substitution {
                    underlying: Subshell {
                        expressions: vec![Expr::Call(Call {
                            arguments: vec![
                                literal(source, "ls"),
                                Expr::TemplateString(TemplateString {
                                    parts: vec![
                                        Expr::Substitution(Substitution {
                                            underlying: Subshell {
                                                expressions: vec![Expr::Call(Call {
                                                    arguments: vec![literal(source, "pwd")],
                                                })],
                                                segment: find_in(source, "$(pwd)"),
                                            },
                                            kind: SubstitutionKind::Capture,
                                        }),
                                        literal(source, "/test"),
                                    ],
                                    segment: find_in(source, "\"$(pwd)/test\""),
                                }),
                            ],
                        })],
                        segment: find_in(source, "$( ls \"$(pwd)/test\" )"),
                    },
                    kind: SubstitutionKind::Capture,
                }),
            ],
        })]
    );
}

#[test]
fn pipe_expressions() {
    let source = "find . | while read -r filename { echo $filename }";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Pipeline(Pipeline {
            commands: vec![
                Expr::Call(Call {
                    arguments: vec![literal(source, "find"), literal(source, "."),],
                }),
                Expr::While(While {
                    condition: Box::new(Expr::Call(Call {
                        arguments: vec![
                            literal(source, "read"),
                            literal(source, "-r"),
                            literal(source, "filename"),
                        ],
                    })),
                    body: Box::new(Expr::Block(Block {
                        expressions: vec![Expr::Call(Call {
                            arguments: vec![
                                literal(source, "echo"),
                                Expr::VarReference(VarReference {
                                    name: VarName::User("filename".into()),
                                    segment: find_in(source, "$filename"),
                                }),
                            ],
                        })],
                        segment: find_in(source, "{ echo $filename }"),
                    })),
                    segment: find_in(source, "while read -r filename { echo $filename }"),
                })
            ],
        })]
    );
}

#[test]
fn pipe_to_command() {
    let source = "{ echo '1\n2' } | cat";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Pipeline(Pipeline {
            commands: vec![
                Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![literal(source, "echo"), literal(source, "'1\n2'")],
                    })],
                    segment: find_in(source, "{ echo '1\n2' }"),
                }),
                Expr::Call(Call {
                    arguments: vec![literal(source, "cat")],
                }),
            ],
        })]
    );
}

#[test]
fn tilde_executable() {
    let source = "~/.local/bin/ls ~+/~ ~user/bar";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::TemplateString(TemplateString {
                    parts: vec![
                        Expr::Tilde(TildeExpansion {
                            structure: Tilde::HomeDir(None),
                            segment: find_in(source, "~"),
                        }),
                        Expr::Literal(Literal {
                            parsed: "/.local/bin/ls".into(),
                            segment: find_in(source, "/.local/bin/ls"),
                        }),
                    ],
                    segment: find_in(source, "~/.local/bin/ls"),
                }),
                Expr::TemplateString(TemplateString {
                    parts: vec![
                        Expr::Tilde(TildeExpansion {
                            structure: Tilde::WorkingDir,
                            segment: find_in(source, "~+"),
                        }),
                        literal(source, "/~"),
                    ],
                    segment: find_in(source, "~+/~"),
                }),
                Expr::TemplateString(TemplateString {
                    parts: vec![
                        Expr::Tilde(TildeExpansion {
                            structure: Tilde::HomeDir(Some(Box::new(literal(source, "user")))),
                            segment: find_in(source, "~user"),
                        }),
                        literal(source, "/bar"),
                    ],
                    segment: find_in(source, "~user/bar"),
                })
            ],
        })]
    );
}

#[test]
fn empty_return() {
    let source = "{ return ; return}";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Block(Block {
            expressions: vec![
                Expr::Return(Return {
                    expr: None,
                    segment: find_in(source, "return"),
                }),
                Expr::Return(Return {
                    expr: None,
                    segment: find_in_nth(source, "return", 1),
                }),
            ],
            segment: source.segment(),
        })]
    );
}

#[test]
fn loop_assign() {
    let source = "loop $a = $(pgrep shell 2>/dev/null)";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Loop(Loop {
            body: Box::new(Expr::Assign(Assign {
                left: Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("a".into()),
                    segment: find_in(source, "$a"),
                })),
                operator: AssignOperator::Assign,
                value: Box::new(Expr::Substitution(Substitution {
                    underlying: Subshell {
                        expressions: vec![Expr::Redirected(Redirected {
                            expr: Box::new(Expr::Call(Call {
                                arguments: vec![literal(source, "pgrep"), literal(source, "shell"),],
                            })),
                            redirections: vec![Redir {
                                fd: RedirFd::Fd(2),
                                operator: RedirOp::Write,
                                operand: literal(source, "/dev/null"),
                                segment: find_in(source, "2>/dev/null"),
                            }],
                        })],
                        segment: find_in(source, "$(pgrep shell 2>/dev/null)"),
                    },
                    kind: SubstitutionKind::Capture,
                })),
            })),
            segment: source.segment(),
        })]
    );
}

#[test]
fn here_string_pipeline() {
    let source = "sort <<< $dict | uniq";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Pipeline(Pipeline {
            commands: vec![
                Expr::Redirected(Redirected {
                    expr: Box::new(Expr::Call(Call {
                        arguments: vec![literal(source, "sort")],
                    })),
                    redirections: vec![Redir {
                        fd: RedirFd::Default,
                        operator: RedirOp::String,
                        operand: Expr::VarReference(VarReference {
                            name: VarName::User("dict".into()),
                            segment: find_in(source, "$dict"),
                        }),
                        segment: find_in(source, "<<< $dict"),
                    }],
                }),
                Expr::Call(Call {
                    arguments: vec![literal(source, "uniq")],
                }),
            ],
        })]
    );
}

#[test]
fn redirect_string_local_exe() {
    let source = "./target/debug/cli <<< 'cat <<< \"hello\"'";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Redirected(Redirected {
            expr: Box::new(Expr::Call(Call {
                arguments: vec![literal(source, "./target/debug/cli")],
            })),
            redirections: vec![Redir {
                fd: RedirFd::Default,
                operator: RedirOp::String,
                operand: literal(source, "'cat <<< \"hello\"'",),
                segment: find_in(source, "<<< 'cat <<< \"hello\"'"),
            }],
        })]
    )
}

#[test]
fn divide_in_statement() {
    let source = "$n/2";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Binary(BinaryOperation {
            left: Box::new(Expr::VarReference(VarReference {
                name: VarName::User("n".into()),
                segment: find_in(source, "$n"),
            })),
            op: BinaryOperator::Divide,
            right: Box::new(Expr::Literal(Literal {
                parsed: 2.into(),
                segment: find_in(source, "2"),
            })),
        })]
    );
}

#[test]
fn inner_var_ref() {
    let source = "dest = \"$DEST/shard_$SHARD_NUMBER/$L\"";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Assign(Assign {
            left: Box::new(Expr::Path(Path {
                path: vec![InclusionPathItem::Symbol(identifier(source, "dest"))],
            })),
            operator: AssignOperator::Assign,
            value: Box::new(Expr::TemplateString(TemplateString {
                parts: vec![
                    Expr::VarReference(VarReference {
                        name: VarName::User("DEST".into()),
                        segment: find_in(source, "$DEST"),
                    }),
                    literal(source, "/shard_"),
                    Expr::VarReference(VarReference {
                        name: VarName::User("SHARD_NUMBER".into()),
                        segment: find_in(source, "$SHARD_NUMBER"),
                    }),
                    literal_nth(source, "/", 1),
                    Expr::VarReference(VarReference {
                        name: VarName::User("L".into()),
                        segment: find_in(source, "$L"),
                    }),
                ],
                segment: find_between(source, "\"", "\""),
            })),
        })]
    );
}

#[test]
fn variable_without_initializer() {
    let source = "var bar; $bar";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Var,
                var: TypedVariable {
                    name: identifier(source, "bar"),
                    ty: None,
                },
                initializer: None,
                segment: find_in(source, "var bar"),
            }),
            Expr::VarReference(VarReference {
                name: VarName::User("bar".into()),
                segment: find_in(source, "$bar"),
            })
        ]
    );
}

#[test]
fn short_variable_increment() {
    let source = "$test += '1' = 2";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Assign(Assign {
            left: Box::new(Expr::VarReference(VarReference {
                name: VarName::User("test".into()),
                segment: find_in(source, "$test"),
            })),
            operator: AssignOperator::Increment,
            value: Box::new(Expr::Assign(Assign {
                left: Box::new(literal(source, "'1'")),
                operator: AssignOperator::Assign,
                value: Box::new(Expr::Literal(Literal {
                    parsed: 2.into(),
                    segment: find_in(source, "2"),
                })),
            })),
        })]
    );
}

#[test]
fn subscript_call() {
    let source = "$foo[0]()[1..=4]";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Subscript(Subscript {
            target: Box::new(Expr::MethodCall(MethodCall {
                source: Box::new(Expr::Subscript(Subscript {
                    target: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("foo".into()),
                        segment: find_in(source, "$foo"),
                    })),
                    index: Box::new(Expr::Literal(Literal {
                        parsed: 0.into(),
                        segment: find_in(source, "0"),
                    })),
                    segment: find_in(source, "$foo[0]"),
                })),
                name: None,
                arguments: Vec::new(),
                type_parameters: Vec::new(),
                segment: find_in(source, "$foo[0]()"),
            })),
            index: Box::new(Expr::Range(Iterable::Range(ast::range::NumericRange {
                start: Box::new(Expr::Literal(Literal {
                    parsed: 1.into(),
                    segment: find_in(source, "1"),
                })),
                end: Box::new(Expr::Literal(Literal {
                    parsed: 4.into(),
                    segment: find_in(source, "4"),
                })),
                step: None,
                upper_inclusive: true,
            }))),
            segment: source.segment(),
        })]
    );
}

#[test]
fn call_subscript() {
    let source = "id()[1] + 2";
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Binary(BinaryOperation {
            left: Box::new(Expr::Subscript(Subscript {
                target: Box::new(Expr::ProgrammaticCall(ProgrammaticCall {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "id"))],
                    arguments: vec![],
                    type_parameters: vec![],
                    segment: find_in(source, "id()"),
                })),
                index: Box::new(Expr::Literal(Literal {
                    parsed: 1.into(),
                    segment: find_in(source, "1"),
                })),
                segment: find_in(source, "id()[1]"),
            })),
            op: BinaryOperator::Plus,
            right: Box::new(Expr::Literal(Literal {
                parsed: 2.into(),
                segment: find_in(source, "2"),
            })),
        })]
    );
}

#[test]
fn argument_dir_wildcard() {
    let source = "ls /tmp/$foo*";
    assert_eq!(
        parse(source).expect("Failed to parse"),
        vec![Expr::Call(Call {
            arguments: vec![
                literal(source, "ls"),
                Expr::Range(Iterable::Files(FilePattern {
                    pattern: Box::new(Expr::TemplateString(TemplateString {
                        parts: vec![
                            literal(source, "/tmp/"),
                            Expr::VarReference(VarReference {
                                name: VarName::User("foo".into()),
                                segment: find_in(source, "$foo"),
                            }),
                            literal(source, "*"),
                        ],
                        segment: find_in(source, "/tmp/$foo*"),
                    })),
                    segment: find_in(source, "/tmp/$foo*"),
                })),
            ],
        })]
    );
}

#[test]
fn variable_name_is_null_byte() {
    let source = "var \0>\0&";
    let parsed: Result<_, ParseError> = parse(source).into();
    assert_eq!(
        parsed,
        Err(ParseError {
            message: "Expected name.".to_owned(),
            position: 4..5,
            kind: ParseErrorKind::Unexpected,
        })
    );
}
