use analyzer::{append_source, Database, Reef};
use miette::{Context, IntoDiagnostic};
use nu_ansi_term::Color;
use reedline::{
    default_emacs_keybindings, ColumnarMenu, Emacs, ExampleHighlighter, FileBackedHistory, KeyCode,
    KeyModifiers, MenuBuilder, PromptEditMode, PromptHistorySearch, PromptHistorySearchStatus,
    Reedline, ReedlineEvent, ReedlineMenu, Signal, ValidationResult, Validator,
};
use std::borrow::Cow;
use std::ffi::OsString;
use std::io::{self, BufRead, IsTerminal, StdinLock};

use cli::project_dir;
use lexer::is_unterminated;

use crate::cli::{use_pipeline, Cli};
use crate::complete::MoshellCompleter;
use crate::pipeline::{Pipeline, PipelineStatus};
use crate::terminal::acquire_terminal;

/// Indefinitely prompts a new expression from stdin and executes it.
pub(crate) fn repl(
    config: &Cli,
    database: &mut Database,
    pipeline: &mut Pipeline,
) -> miette::Result<PipelineStatus> {
    let mut reef = Reef::new(OsString::from("stdin"));

    let mut editor = if io::stdin().is_terminal() && cfg!(not(miri)) {
        #[cfg(unix)]
        pipeline.vm.set_pgid(acquire_terminal().as_raw());
        Editor::LineEditor(Box::new(editor().context("Could not start REPL")?))
    } else {
        Editor::NoEditor(MultilineInput::new(io::stdin().lock()))
    };

    let mut status = PipelineStatus::Success;

    loop {
        let line = editor.read_line(&Prompt);

        match line {
            Ok(Signal::Success(source)) => {
                let path = pipeline.filesystem.add(&source);
                let errors =
                    append_source(database, &mut reef, &pipeline.filesystem, path, &source);
                status = status.compose(use_pipeline(database, &reef, pipeline, errors, config));
                reef.clear_cache();
            }
            Ok(Signal::CtrlC) => eprintln!("^C"),
            Ok(Signal::CtrlD) => break Ok(status),
            Err(err) => {
                eprintln!("Error: {err:?}");
                break Ok(status);
            }
        }
    }
}

/// The REPL editor.
enum Editor<'a> {
    /// An interactive line editor.
    LineEditor(Box<Reedline>),

    /// A simple stdin reader for a non interactive mode.
    NoEditor(MultilineInput<'a>),
}

impl Editor<'_> {
    fn read_line(&mut self, prompt: &Prompt) -> io::Result<Signal> {
        match self {
            Editor::LineEditor(editor) => editor.read_line(prompt),
            Editor::NoEditor(stdin) => stdin.read_line(),
        }
    }
}

fn editor() -> miette::Result<Reedline> {
    let mut highlighter = ExampleHighlighter::default();
    highlighter.change_colors(Color::Default, Color::Default, Color::Default);
    let mut editor = Reedline::create()
        .with_validator(Box::new(TerminatedValidator))
        .with_highlighter(Box::new(highlighter));
    if let Some(project_dir) = project_dir() {
        let history_path = project_dir.data_dir().join("history.txt");
        let history = Box::new(
            FileBackedHistory::with_file(4000, history_path.clone())
                .into_diagnostic()
                .with_context(|| {
                    format!("Could not open history file: {}", history_path.display())
                })?,
        );
        editor = editor.with_history(history);
    }
    let completion_menu = Box::new(
        ColumnarMenu::default()
            .with_text_style(Color::Default.normal())
            .with_selected_text_style(Color::Default.bold().on(Color::Green))
            .with_name("completion_menu"),
    );
    let mut keybindings = default_emacs_keybindings();
    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Tab,
        ReedlineEvent::UntilFound(vec![
            ReedlineEvent::Menu("completion_menu".to_owned()),
            ReedlineEvent::MenuNext,
        ]),
    );
    let edit_mode = Box::new(Emacs::new(keybindings));
    editor = editor
        .with_menu(ReedlineMenu::EngineCompleter(completion_menu))
        .with_edit_mode(edit_mode)
        .with_quick_completions(true)
        .with_completer(Box::new(MoshellCompleter));
    Ok(editor)
}

struct Prompt;

impl reedline::Prompt for Prompt {
    fn render_prompt_left(&self) -> Cow<str> {
        Cow::Borrowed("=> ")
    }

    fn render_prompt_right(&self) -> Cow<str> {
        Cow::Borrowed("")
    }

    fn render_prompt_indicator(&self, _: PromptEditMode) -> Cow<str> {
        Cow::Borrowed("")
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        Cow::Borrowed("... ")
    }

    fn render_prompt_history_search_indicator(
        &self,
        history_search: PromptHistorySearch,
    ) -> Cow<str> {
        match history_search.status {
            PromptHistorySearchStatus::Passing => Cow::Borrowed("(reverse-search): "),
            PromptHistorySearchStatus::Failing => Cow::Borrowed("(failed reverse-search): "),
        }
    }
}

struct TerminatedValidator;

impl Validator for TerminatedValidator {
    fn validate(&self, line: &str) -> ValidationResult {
        match is_unterminated(line) {
            true => ValidationResult::Incomplete,
            false => ValidationResult::Complete,
        }
    }
}

struct MultilineInput<'a> {
    buf: StdinLock<'a>,
    validator: TerminatedValidator,
}

impl<'a> MultilineInput<'a> {
    fn new(buf: StdinLock<'a>) -> Self {
        Self {
            buf,
            validator: TerminatedValidator,
        }
    }

    fn read_line(&mut self) -> io::Result<Signal> {
        let mut line = String::new();
        loop {
            // Read a terminated expression from stdin.
            match self.buf.read_line(&mut line)? {
                0 => return Ok(Signal::CtrlD),
                _ => {
                    match self.validator.validate(&line) {
                        ValidationResult::Complete => return Ok(Signal::Success(line)),
                        ValidationResult::Incomplete => {}
                    }
                    if !line.ends_with('\n') {
                        line.push('\n');
                    }
                }
            }
        }
    }
}
