use crate::cli::handle_source;
use crate::report::print_flush;
use context::source::{OwnedSource, Source};
use parser::parse;
use std::io;
use std::io::BufRead;
use std::io::Write;

/// Indefinitely prompts a new expression to the stdin,
/// displaying back the errors if any and the formed AST
pub fn prompt() {
    loop {
        if let Some(source) = parse_input() {
            handle_source(source.as_source());
        }
    }
}

/// Parses stdin until the user's input forms a source code with no unclosed delimiters
/// and return the source.
fn parse_input() -> Option<OwnedSource> {
    let stdin = io::stdin();
    let lines = stdin.lock().lines();
    let mut content = String::new();
    print_flush!("=> ");
    for line in lines {
        let line = line.expect("couldn't read line from stdin");
        content.push_str(&line);
        if line.ends_with('\\') {
            content.push('\n');
            print_flush!("-> ");
            continue;
        }

        let source = Source::new(&content, "stdin");
        let report = parse(source);
        if !report.stack_ended {
            content.push('\n');
            print_flush!("-> ");
            continue; // Silently ignore incomplete input
        }

        return Some(OwnedSource::new(source.source.to_string(), source.name.to_string()));
    }
    None
}
