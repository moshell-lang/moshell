use crate::report::{print_flush};
use context::source::{OwnedSource};
use parser::parse;
use std::io;
use std::io::{BufRead};
use std::io::Write;
use crate::cli::handle_source;


pub fn prompt() {
    loop {
        if let Some(source) = parse_input() {
            handle_source(source);
        }
    }
}

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
            print_flush!(".. ");
            continue;
        }

        let source = OwnedSource::new(content.clone(), "stdin".to_string());
        let report = parse(source.as_source());
        if !report.stack_ended {
            content.push('\n');
            print_flush!(".. ");
            continue; // Silently ignore incomplete input
        }

        return Some(source);
    }
    None
}