use miette::{Context, IntoDiagnostic};
use nu_ansi_term::Color;
use reedline::{
    default_emacs_keybindings, ColumnarMenu, Emacs, ExampleHighlighter, FileBackedHistory, KeyCode,
    KeyModifiers, PromptEditMode, PromptHistorySearch, PromptHistorySearchStatus, Reedline,
    ReedlineEvent, ReedlineMenu, Signal, ValidationResult, Validator,
};
use std::borrow::Cow;
use std::io::{self, BufRead, IsTerminal, StdinLock};
use std::path::PathBuf;

use analyzer::importer::ImportResult;
use analyzer::name::Name;
use analyzer::reef::Externals;
use analyzer::relations::SourceId;
use analyzer::{Analyzer, Inject};
use cli::project_dir;
use compiler::externals::CompilerExternals;
use context::source::OwnedSource;
use lexer::is_unterminated;
use vm::VM;

use crate::cli::{use_pipeline, Cli};
use crate::complete::MoshellCompleter;
use crate::pipeline::{ErrorReporter, PipelineStatus, SourcesCache};

/// Indefinitely prompts a new expression from stdin and executes it.
pub(crate) fn repl(
    dir: PathBuf,
    config: &Cli,
    mut sources: SourcesCache,
    externals: Externals,
    compiler_externals: CompilerExternals,
    mut vm: VM,
) -> miette::Result<PipelineStatus> {
    let mut analyzer = Analyzer::new();
    sources.register(dir);

    let mut editor = if io::stdin().is_terminal() {
        Editor::LineEditor(Box::new(editor().context("Could not start REPL")?))
    } else {
        Editor::NoEditor(MultilineInput::new(io::stdin().lock()))
    };

    let mut status = PipelineStatus::Success;

    // Keep track of the previous attributed source, so that we can inject
    // the next one into the same context.
    let mut starting_source: Option<SourceId> = None;
    let name = Name::new("stdin");

    loop {
        let line = editor.read_line(&Prompt);

        match line {
            Ok(Signal::Success(source)) => {
                let source = OwnedSource::new(source, "stdin".to_owned());
                let importer = sources.last_mut();
                if let ImportResult::Success(imported) = importer.insert(source) {
                    let mut analysis = analyzer.inject(
                        Inject {
                            name: name.clone(),
                            imported,
                            attached: starting_source,
                        },
                        importer,
                        &externals,
                    );

                    // Reuse the same diagnotics by moving them, requiring to keep track
                    // if there was any error since they will be consumed before being
                    // able to cancel the analysis (the errors need the context that is
                    // dropped when the analysis is reverted).
                    let diagnostics = analysis.take_diagnostics();
                    let is_ready = diagnostics.is_empty();

                    let errors = importer.take_errors();
                    status = status.compose(
                        use_pipeline(
                            &name,
                            analysis.attributed_id(),
                            analysis.analyzer(),
                            &externals,
                            &compiler_externals,
                            &mut vm,
                            diagnostics,
                            errors,
                            &sources,
                            config,
                        )
                        .err()
                        .unwrap_or(PipelineStatus::Success),
                    );

                    // Remember the successfully injected source, or revert the analysis.
                    if is_ready {
                        starting_source = Some(analysis.attributed_id());
                    } else {
                        analysis.revert();
                    }
                } else {
                    // Probably hit some parse errors, so we skip any further analysis and
                    // directly display the errors. There should be no actual diagnostics
                    // in the pipeline, but we consume them anyway to reuse the same
                    // end-of-pipeline logic.
                    let diagnostics = analyzer.take_diagnostics();
                    status = status.compose(
                        use_pipeline(
                            &name,
                            SourceId(0), // this value has no importance
                            &analyzer,
                            &externals,
                            &compiler_externals,
                            &mut vm,
                            diagnostics,
                            importer.take_errors(),
                            &sources,
                            config,
                        )
                        .err()
                        .unwrap_or(PipelineStatus::Success),
                    );
                }
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
