use pretty_assertions::assert_eq;

use ast::call::{
    Call, MethodCall, Pipeline, ProgrammaticCall, Redir, RedirFd, RedirOp, Redirected,
};
use ast::control_flow::{Loop, While};
use ast::function::Return;
use ast::group::{Block, Subshell};
use ast::operation::{BinaryOperation, BinaryOperator};
use ast::r#use::InclusionPathItem;
use ast::range::{Iterable, Subscript};
use ast::substitution::{Substitution, SubstitutionKind};
use ast::value::{Literal, TemplateString};
use ast::variable::{
    Assign, AssignOperator, Identifier, TypedVariable, VarDeclaration, VarKind, VarName,
    VarReference,
};
use ast::Expr;
use context::source::{Source, SourceSegmentHolder};
use context::str_find::{find_between, find_in, find_in_nth};
use parser::parse;
use parser::source::{literal, literal_nth};

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
            arguments: vec![
                literal(source.source, "echo"),
                literal(source.source, "'$var5'"),
                Expr::VarReference(VarReference {
                    name: VarName::User("var5"),
                    segment: find_in_nth(source.source, "$var5", 1),
                }),
            ],
        })]
    );
}

#[test]
fn with_lexer_var_reference_two() {
    let source = Source::unknown("`fake$cmd` do $arg2");
    let parsed = parse(source).expect("Failed to parse");

    assert_eq!(
        parsed,
        vec![Expr::Call(Call {
            arguments: vec![
                Expr::TemplateString(TemplateString {
                    parts: vec![
                        literal(source.source, "fake"),
                        Expr::VarReference(VarReference {
                            name: VarName::User("cmd"),
                            segment: find_in(source.source, "$cmd"),
                        }),
                    ],
                    segment: find_in(source.source, "`fake$cmd`"),
                }),
                literal(source.source, "do"),
                Expr::VarReference(VarReference {
                    name: VarName::User("arg2"),
                    segment: find_in(source.source, "$arg2"),
                }),
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
                literal(source.source, "echo"),
                Expr::TemplateString(TemplateString {
                    parts: vec![
                        literal(source.source, "hello "),
                        Expr::VarReference(VarReference {
                            name: VarName::User("world"),
                            segment: find_in(source.source, "$world"),
                        }),
                        literal(source.source, " everyone "),
                        Expr::VarReference(VarReference {
                            name: VarName::User("verb"),
                            segment: find_in(source.source, "$verb"),
                        }),
                        Expr::VarReference(VarReference {
                            name: VarName::User("ready"),
                            segment: find_in(source.source, "${ready}"),
                        }),
                        literal(source.source, "!"),
                    ],
                    segment: find_between(source.source, "\"", "\""),
                }),
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
                arguments: vec![literal(source.source, "test")],
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
                arguments: vec![literal(source.source, "command")],
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
                    arguments: vec![literal(source.source, "ls"), literal(source.source, "-l")],
                }),
                Expr::Redirected(Redirected {
                    expr: Box::new(Expr::Call(Call {
                        arguments: vec![
                            literal(source.source, "grep"),
                            literal(source.source, "'hello'"),
                        ],
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
                    arguments: vec![literal(source.source, "ls")],
                }),
                Expr::Call(Call {
                    arguments: vec![literal(source.source, "wc")],
                }),
                Expr::Call(Call {
                    arguments: vec![
                        literal(source.source, "tr"),
                        literal(source.source, "-s"),
                        literal(source.source, "' '"),
                    ],
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
                arguments: vec![literal(content, "grep"), literal_nth(content, "e", 1)],
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
            arguments: vec![
                literal(source.source, "echo"),
                Expr::Substitution(Substitution {
                    underlying: Subshell {
                        expressions: vec![Expr::Call(Call {
                            arguments: vec![
                                literal(source.source, "ls"),
                                literal(source.source, "-l"),
                            ],
                        })],
                        segment: find_in(source.source, "$(ls -l)"),
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
                literal(source.source, "echo"),
                Expr::Substitution(Substitution {
                    underlying: Subshell {
                        expressions: vec![Expr::Call(Call {
                            arguments: vec![
                                literal(source.source, "ls"),
                                Expr::TemplateString(TemplateString {
                                    parts: vec![
                                        Expr::Substitution(Substitution {
                                            underlying: Subshell {
                                                expressions: vec![Expr::Call(Call {
                                                    arguments: vec![literal(source.source, "pwd")],
                                                })],
                                                segment: find_in(source.source, "$(pwd)"),
                                            },
                                            kind: SubstitutionKind::Capture,
                                        }),
                                        literal(source.source, "/test"),
                                    ],
                                    segment: find_in(source.source, "\"$(pwd)/test\""),
                                }),
                            ],
                        })],
                        segment: find_in(source.source, "$( ls \"$(pwd)/test\" )"),
                    },
                    kind: SubstitutionKind::Capture,
                }),
            ],
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
                    arguments: vec![literal(source.source, "find"), literal(source.source, "."),],
                }),
                Expr::While(While {
                    condition: Box::new(Expr::Call(Call {
                        arguments: vec![
                            literal(source.source, "read"),
                            literal(source.source, "-r"),
                            literal(source.source, "filename"),
                        ],
                    })),
                    body: Box::new(Expr::Block(Block {
                        expressions: vec![Expr::Call(Call {
                            arguments: vec![
                                literal(source.source, "echo"),
                                Expr::VarReference(VarReference {
                                    name: VarName::User("filename"),
                                    segment: find_in(source.source, "$filename"),
                                }),
                            ],
                        })],
                        segment: find_in(source.source, "{ echo $filename }"),
                    })),
                    segment: find_in(source.source, "while read -r filename { echo $filename }"),
                })
            ],
        })]
    );
}

#[test]
fn pipe_to_command() {
    let source = Source::unknown("{ echo '1\n2' } | cat");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Pipeline(Pipeline {
            commands: vec![
                Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![
                            literal(source.source, "echo"),
                            literal(source.source, "'1\n2'")
                        ],
                    })],
                    segment: find_in(source.source, "{ echo '1\n2' }"),
                }),
                Expr::Call(Call {
                    arguments: vec![literal(source.source, "cat")],
                }),
            ],
        })]
    );
}

#[test]
fn empty_return() {
    let source = Source::unknown("{ return ; return}");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Block(Block {
            expressions: vec![
                Expr::Return(Return {
                    expr: None,
                    segment: find_in(source.source, "return"),
                }),
                Expr::Return(Return {
                    expr: None,
                    segment: find_in_nth(source.source, "return", 1),
                }),
            ],
            segment: source.segment(),
        })]
    );
}

#[test]
fn loop_assign() {
    let source = Source::unknown("loop $a = $(pgrep shell 2>/dev/null)");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Loop(Loop {
            body: Box::new(Expr::Assign(Assign {
                left: Box::new(Expr::VarReference(VarReference {
                    name: VarName::User("a"),
                    segment: find_in(source.source, "$a"),
                })),
                operator: AssignOperator::Assign,
                value: Box::new(Expr::Substitution(Substitution {
                    underlying: Subshell {
                        expressions: vec![Expr::Redirected(Redirected {
                            expr: Box::new(Expr::Call(Call {
                                arguments: vec![
                                    literal(source.source, "pgrep"),
                                    literal(source.source, "shell"),
                                ],
                            })),
                            redirections: vec![Redir {
                                fd: RedirFd::Fd(2),
                                operator: RedirOp::Write,
                                operand: literal(source.source, "/dev/null"),
                                segment: find_in(source.source, "2>/dev/null"),
                            }],
                        })],
                        segment: find_in(source.source, "$(pgrep shell 2>/dev/null)"),
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
    let source = Source::unknown("sort <<< $dict | uniq");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Pipeline(Pipeline {
            commands: vec![
                Expr::Redirected(Redirected {
                    expr: Box::new(Expr::Call(Call {
                        arguments: vec![literal(source.source, "sort")],
                    })),
                    redirections: vec![Redir {
                        fd: RedirFd::Default,
                        operator: RedirOp::String,
                        operand: Expr::VarReference(VarReference {
                            name: VarName::User("dict"),
                            segment: find_in(source.source, "$dict"),
                        }),
                        segment: find_in(source.source, "<<< $dict"),
                    }],
                }),
                Expr::Call(Call {
                    arguments: vec![literal(source.source, "uniq")],
                }),
            ],
        })]
    );
}

#[test]
fn redirect_string_local_exe() {
    let source = Source::unknown("./target/debug/cli <<< 'cat <<< \"hello\"'");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Redirected(Redirected {
            expr: Box::new(Expr::Call(Call {
                arguments: vec![literal(source.source, "./target/debug/cli")],
            })),
            redirections: vec![Redir {
                fd: RedirFd::Default,
                operator: RedirOp::String,
                operand: literal(source.source, "'cat <<< \"hello\"'",),
                segment: find_in(source.source, "<<< 'cat <<< \"hello\"'"),
            }],
        })]
    )
}

#[test]
fn divide_in_statement() {
    let source = Source::unknown("$n/2");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Binary(BinaryOperation {
            left: Box::new(Expr::VarReference(VarReference {
                name: VarName::User("n"),
                segment: find_in(source.source, "$n"),
            })),
            op: BinaryOperator::Divide,
            right: Box::new(Expr::Literal(Literal {
                parsed: 2.into(),
                segment: find_in(source.source, "2"),
            })),
        })]
    );
}

#[test]
fn inner_var_ref() {
    let source = Source::unknown("dest = \"$DEST/shard_$SHARD_NUMBER/$L\"");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Assign(Assign {
            left: Box::new(Expr::Identifier(Identifier {
                name: "dest",
                segment: find_in(source.source, "dest"),
            })),
            operator: AssignOperator::Assign,
            value: Box::new(Expr::TemplateString(TemplateString {
                parts: vec![
                    Expr::VarReference(VarReference {
                        name: VarName::User("DEST"),
                        segment: find_in(source.source, "$DEST"),
                    }),
                    literal(source.source, "/shard_"),
                    Expr::VarReference(VarReference {
                        name: VarName::User("SHARD_NUMBER"),
                        segment: find_in(source.source, "$SHARD_NUMBER"),
                    }),
                    literal_nth(source.source, "/", 1),
                    Expr::VarReference(VarReference {
                        name: VarName::User("L"),
                        segment: find_in(source.source, "$L"),
                    }),
                ],
                segment: find_between(source.source, "\"", "\""),
            })),
        })]
    );
}

#[test]
fn variable_without_initializer() {
    let source = Source::unknown("var bar; $bar");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Var,
                var: TypedVariable {
                    name: "bar",
                    ty: None,
                    segment: find_in(source.source, "bar"),
                },
                initializer: None,
                segment: find_in(source.source, "var bar"),
            }),
            Expr::VarReference(VarReference {
                name: VarName::User("bar"),
                segment: find_in(source.source, "$bar"),
            })
        ]
    );
}

#[test]
fn short_variable_increment() {
    let source = Source::unknown("$test += '1' = 2");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Assign(Assign {
            left: Box::new(Expr::VarReference(VarReference {
                name: VarName::User("test"),
                segment: find_in(source.source, "$test"),
            })),
            operator: AssignOperator::Increment,
            value: Box::new(Expr::Assign(Assign {
                left: Box::new(literal(source.source, "'1'")),
                operator: AssignOperator::Assign,
                value: Box::new(Expr::Literal(Literal {
                    parsed: 2.into(),
                    segment: find_in(source.source, "2"),
                })),
            })),
        })]
    );
}

#[test]
fn subscript_call() {
    let source = Source::unknown("$foo[0]()[1..=4]");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Subscript(Subscript {
            target: Box::new(Expr::MethodCall(MethodCall {
                source: Box::new(Expr::Subscript(Subscript {
                    target: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("foo"),
                        segment: find_in(source.source, "$foo"),
                    })),
                    index: Box::new(Expr::Literal(Literal {
                        parsed: 0.into(),
                        segment: find_in(source.source, "0"),
                    })),
                    segment: find_in(source.source, "$foo[0]"),
                })),
                name: None,
                arguments: Vec::new(),
                type_parameters: Vec::new(),
                segment: find_in(source.source, "$foo[0]()"),
            })),
            index: Box::new(Expr::Range(Iterable::Range(ast::range::NumericRange {
                start: Box::new(Expr::Literal(Literal {
                    parsed: 1.into(),
                    segment: find_in(source.source, "1"),
                })),
                end: Box::new(Expr::Literal(Literal {
                    parsed: 4.into(),
                    segment: find_in(source.source, "4"),
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
    let source = Source::unknown("id()[1] + 2");
    let parsed = parse(source).expect("Failed to parse");
    assert_eq!(
        parsed,
        vec![Expr::Binary(BinaryOperation {
            left: Box::new(Expr::Subscript(Subscript {
                target: Box::new(Expr::ProgrammaticCall(ProgrammaticCall {
                    path: vec![InclusionPathItem::Symbol(
                        "id",
                        find_in(source.source, "id")
                    )],
                    arguments: vec![],
                    type_parameters: vec![],
                    segment: find_in(source.source, "id()"),
                })),
                index: Box::new(Expr::Literal(Literal {
                    parsed: 1.into(),
                    segment: find_in(source.source, "1"),
                })),
                segment: find_in(source.source, "id()[1]"),
            })),
            op: BinaryOperator::Plus,
            right: Box::new(Expr::Literal(Literal {
                parsed: 2.into(),
                segment: find_in(source.source, "2"),
            })),
        })]
    );
}
