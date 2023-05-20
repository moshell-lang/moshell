use std::process::exit;
use context::source::{OwnedSource};
use parser::parse;
use rustyline::{Cmd, ColorMode, DefaultEditor, Editor, Event, EventHandler, KeyCode, KeyEvent, Modifiers};
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::history::DefaultHistory;
use crate::cli::handle_source;

type REPLEditor = Editor<(), DefaultHistory>;

/// Indefinitely prompts a new expression to the stdin,
/// displaying back the errors if any and the formed AST
pub fn prompt() {
    let mut editor: REPLEditor = DefaultEditor::new().expect("unable to instantiate terminal editor");
    editor.set_color_mode(ColorMode::Enabled);
    editor.set_history_ignore_dups(true).unwrap();
    editor.set_history_ignore_space(true);
    editor.bind_sequence(Event::KeySeq(vec![KeyEvent(KeyCode::Char('u'), Modifiers::ALT)]), EventHandler::from(Cmd::Undo(1)));
    loop {
        let source = parse_input(&mut editor);
        handle_source(source);
    }
}

fn strip_indent(line: &mut String) -> String {
    let mut indent = String::new();
    let line_clone = line.to_string();
    let chars = line_clone.chars();
    for c in chars {
        if !c.is_whitespace() {
            return indent;
        }
        line.remove(0);
        indent.push(c);
    }
    indent
}

/// Parses stdin until the user's input forms a source code with no unclosed delimiters
/// and return the source.
fn parse_input(editor: &mut REPLEditor) -> OwnedSource {
    let mut content = String::new();
    let mut prompt_prefix = "=> ".to_string();
    let mut indent_prefix = String::new();
    loop {
        let line = editor.readline_with_initial(&prompt_prefix, (&indent_prefix, ""));
        let mut line = match line {
            Ok(line) => line,
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => exit(1),
            e => e.expect("error when reading next line from editor")
        };

        editor.add_history_entry(line.clone()).expect("terminal has no history");

        indent_prefix = strip_indent(&mut line);

        content.push_str(&line);
        if line.ends_with('\\') {
            content.push('\n');
            prompt_prefix = "-> ".to_string();
            continue;
        }

        let source = OwnedSource::new(content.clone(), "stdin".to_string());
        let report = parse(source.as_source());
        if !report.stack_ended {
            content.push('\n');
            prompt_prefix = "?> ".to_string(); //Todo display unterminated delimiter
            continue; // Silently ignore incomplete input
        }

        return source;
    }
}