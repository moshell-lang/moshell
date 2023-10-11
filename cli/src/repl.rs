use miette::{Context, IntoDiagnostic};
use reedline::{
    FileBackedHistory, PromptEditMode, PromptHistorySearch, PromptHistorySearchStatus, Reedline,
    Signal, ValidationResult,
};
use std::borrow::Cow;
use std::path::PathBuf;

use analyzer::importer::ImportResult;
use analyzer::name::Name;
use analyzer::reef::Externals;
use analyzer::relations::SourceId;
use analyzer::{Analyzer, Inject};
use cli::project_dir;
use context::source::OwnedSource;
use lexer::is_unterminated;
use vm::VM;

use crate::cli::{use_pipeline, Cli};
use crate::pipeline::{ErrorReporter, PipelineStatus, SourcesCache};

/// Indefinitely prompts a new expression from stdin and executes it.
pub(crate) fn repl(
    dir: PathBuf,
    config: &Cli,
    mut sources: SourcesCache,
    externals: Externals,
    mut vm: VM,
) -> miette::Result<PipelineStatus> {
    let mut analyzer = Analyzer::new();
    sources.register(dir);

    let mut editor = editor().context("Could not start REPL")?;

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

                    status = status.compose(use_pipeline(
                        &name,
                        analysis.attributed_id(),
                        analysis.analyzer(),
                        &externals,
                        &mut vm,
                        diagnostics,
                        errors,
                        &sources,
                        config,
                    ));

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
                    status = status.compose(use_pipeline(
                        &name,
                        SourceId(0), // this value has no importance
                        &analyzer,
                        &externals,
                        &mut vm,
                        diagnostics,
                        importer.take_errors(),
                        &sources,
                        config,
                    ));
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

fn editor() -> miette::Result<Reedline> {
    let mut editor = Reedline::create().with_validator(Box::new(TerminatedValidator));
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
            PromptHistorySearchStatus::Passing => Cow::Borrowed("(reverse-i-search)`"),
            PromptHistorySearchStatus::Failing => Cow::Borrowed("(failed reverse-i-search)`"),
        }
    }
}

struct TerminatedValidator;

impl reedline::Validator for TerminatedValidator {
    fn validate(&self, line: &str) -> ValidationResult {
        match is_unterminated(line) {
            true => ValidationResult::Incomplete,
            false => ValidationResult::Complete,
        }
    }
}
