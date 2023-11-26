mod bash;

use ast::value::Literal;
use ast::Expr;
use context::source::Source;
use lexer::token::Token;
use lexer::token::TokenType;
use parser::parse;
use reedline::{Completer, Span, Suggestion};
use std::env;
use std::path::{Path, MAIN_SEPARATOR};

/// A command completer.
pub(crate) struct MoshellCompleter;

impl Completer for MoshellCompleter {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        let (tokens, unmatched) = lexer::lex(&line[..pos]);
        let mut fixed = String::from(&line[..pos]);
        for unmatched in unmatched {
            if unmatched.candidate.is_some() {
                // Avoid completing a context that cannot be determined due to a mismatched token.
                return Vec::new();
            }
            if let Some(opening) = unmatched.opening {
                // Complete the missing token to not confuse the parser.
                match line.as_bytes()[opening] {
                    b'(' => fixed.push(')'),
                    b'[' => fixed.push(']'),
                    b'{' => fixed.push('}'),
                    _ => {}
                };
            }
        }

        let ends_with_whitespace = matches!(
            tokens.last(),
            Some(Token {
                token_type: TokenType::Space,
                ..
            })
        );
        match get_last_word(&fixed, ends_with_whitespace) {
            Some(CompletionCursor::Command(word)) => {
                if word.starts_with('.') {
                    get_files(&word, pos, |path| is_executable::is_executable(path))
                } else {
                    get_executables(&word, pos)
                }
            }
            Some(CompletionCursor::Argument(arguments)) => {
                let Some(word) = arguments.last().unwrap() else {
                    return Vec::new();
                };
                if arguments.first().unwrap().is_none() {
                    return get_files(word, pos, |_| true);
                }
                let word_len = word.len();
                let Ok(bash) = bash::BashComplete::new() else {
                    return get_files(word, pos, |_| true);
                };
                let rendered = arguments
                    .iter()
                    .filter_map(Option::clone)
                    .collect::<Vec<String>>()
                    .join(" ");
                match bash.complete(&rendered) {
                    Ok(Some(suggestions)) => suggestions
                        .into_iter()
                        .map(|value| Suggestion {
                            value,
                            description: None,
                            extra: None,
                            span: Span::new(pos - word_len, pos),
                            append_whitespace: true,
                        })
                        .collect::<Vec<Suggestion>>(),
                    Ok(None) => get_files(word, pos, |_| true),
                    Err(_) => Vec::new(),
                }
            }
            None => Vec::new(),
        }
    }
}

/// The type of completion to output.
#[derive(Debug, Clone)]
enum CompletionCursor {
    /// An executable command.
    Command(String),

    /// A file path argument.
    Argument(Vec<Option<String>>),
}

/// Get the last shell word in the given text.
fn get_last_word(text: &str, next: bool) -> Option<CompletionCursor> {
    let mut report = parse(Source::unknown(text));
    let mut expr = report.expr.pop()?;
    loop {
        match expr {
            Expr::Unary(unary) => {
                expr = *unary.expr;
            }
            Expr::Binary(binary) => {
                expr = *binary.right;
            }
            Expr::Block(mut block) => {
                expr = block.expressions.pop()?;
            }
            Expr::FunctionDeclaration(function) => {
                expr = *function.body?;
            }
            Expr::Call(call) => {
                let mut arguments = call
                    .arguments
                    .iter()
                    .map(get_value)
                    .collect::<Vec<Option<String>>>();
                if next {
                    arguments.push(Some("".to_owned()));
                }
                return Some(match arguments.as_slice() {
                    [command] => CompletionCursor::Command(command.clone()?),
                    _ => CompletionCursor::Argument(arguments),
                });
            }
            Expr::TemplateString(mut template) => {
                expr = template.parts.pop()?;
            }
            _ => break None,
        }
    }
}

/// Get the value of the given expression.
fn get_value(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Literal(Literal { parsed, .. }) => Some(parsed.to_string()),
        Expr::TemplateString(template) => {
            let mut value = String::new();
            for part in &template.parts {
                value.push_str(&get_value(part)?);
            }
            Some(value)
        }
        _ => None,
    }
}

/// Lists all executables in the current path.
fn get_executables(word: &str, pos: usize) -> Vec<Suggestion> {
    let mut suggestions = Vec::new();
    if let Some(path) = env::var_os("PATH") {
        for path in env::split_paths(&path) {
            if let Ok(dir) = path.read_dir() {
                for entry in dir.filter_map(|e| e.ok()) {
                    if let Some(name) = entry.file_name().to_str() {
                        if name.starts_with(word) && is_executable::is_executable(entry.path()) {
                            suggestions.push(Suggestion {
                                value: name.to_owned(),
                                description: None,
                                extra: None,
                                span: Span::new(pos - word.len(), pos),
                                append_whitespace: true,
                            });
                        }
                    }
                }
            }
        }
    }
    suggestions
}

/// Lists all files in the current path.
fn get_files(word: &str, pos: usize, predicate: fn(&Path) -> bool) -> Vec<Suggestion> {
    let mut suggestions = Vec::new();
    let mut path = Path::new(word);
    if !word.ends_with(MAIN_SEPARATOR) {
        // Complete the last path component, so remove it from the path.
        path = path.parent().unwrap_or(Path::new("."));
    }

    if let Ok(dir) = path.read_dir() {
        // Extract the filename
        let partial = word
            .rsplit_once(MAIN_SEPARATOR)
            .map(|(_, p)| p)
            .unwrap_or(word);

        for entry in dir.filter_map(|e| e.ok()) {
            if !predicate(&entry.path()) {
                continue;
            }
            let mut entry_name = entry.file_name().to_string_lossy().into_owned();
            if entry.file_type().map_or(false, |t| t.is_dir()) {
                entry_name.push(MAIN_SEPARATOR);
            }
            if entry_name.starts_with(partial) {
                suggestions.push(Suggestion {
                    value: entry_name,
                    description: None,
                    extra: None,
                    span: Span::new(pos - partial.len(), pos),
                    append_whitespace: false,
                });
            }
        }
    }
    suggestions
}
