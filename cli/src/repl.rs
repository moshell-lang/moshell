use std::io;
use std::io::BufRead;
use std::io::Write;
use std::path::PathBuf;

use analyzer::importer::ImportResult;
use analyzer::name::Name;
use analyzer::reef::Externals;
use analyzer::relations::SourceId;
use analyzer::{Analyzer, Inject};
use context::source::OwnedSource;
use lexer::is_unterminated;
use vm::VM;

use crate::cli::{use_pipeline, Cli};
use crate::pipeline::{ErrorReporter, FileImporter, PipelineStatus, SourcesCache};
use crate::report::print_flush;

/// Indefinitely prompts a new expression from stdin and executes it.
pub fn repl(
    dir: PathBuf,
    config: &Cli,
    mut sources: SourcesCache,
    externals: Externals,
    mut vm: VM,
) -> PipelineStatus {
    let mut analyzer = Analyzer::new();
    let mut importer = FileImporter::new(dir);

    let mut status = PipelineStatus::Success;

    // Keep track of the previous attributed source, so that we can inject
    // the next one into the same context.
    let mut starting_source: Option<SourceId> = None;
    let name = Name::new("stdin");

    // Loop over the user's input until they exit the REPL.
    while let Some(source) = parse_input() {
        // Inject the source directly into the importer, in order to know
        // its attributed id. It will be used later to inject the next
        // successfully parsed prompt.
        if let ImportResult::Success(imported) = importer.insert(source) {
            let mut analysis = analyzer.inject(
                Inject {
                    name: name.clone(),
                    imported,
                    attached: starting_source,
                },
                &mut importer,
                &externals,
            );

            // Reuse the same diagnotics by moving them, requiring to keep track
            // if there was any error since they will be consumed before being
            // able to cancel the analysis (the errors need the context that is
            // dropped when the analysis is reverted).
            let diagnostics = analysis.take_diagnostics();
            let is_ready = diagnostics.is_empty();

            let errors = importer.take_errors();
            sources.set(externals.current, importer.take_sources());

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
            sources.set(externals.current, importer.take_sources());
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
    status
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

        if is_unterminated(&content) {
            content.push('\n');
            print_flush!("-> ");
            continue;
        }

        return Some(OwnedSource::new(content, "stdin".to_owned()));
    }
    None
}
