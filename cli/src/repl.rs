use std::io;
use std::io::BufRead;
use std::io::Write;

use analyzer::importer::ImportResult;
use analyzer::name::Name;
use analyzer::relations::SourceId;
use analyzer::Inject;
use context::source::OwnedSource;
use lexer::is_unterminated;

use crate::cli::{use_pipeline, Cli};
use crate::pipeline::{FileImporter, Pipeline, PipelineStatus};
use crate::report::print_flush;

/// Indefinitely prompts a new expression from stdin and executes it.
pub fn prompt(mut importer: FileImporter, config: &Cli) -> PipelineStatus {
    // Init a new pipeline that will be used to execute each expression.
    let mut pipeline = Pipeline::new();
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
            let mut analysis = pipeline.analyzer.inject(
                Inject {
                    name: name.clone(),
                    imported,
                    attached: starting_source,
                },
                &mut importer,
            );

            // Reuse the same diagnotics by moving them, requiring to keep track
            // if there was any error since they will be consumed before being
            // able to cancel the analysis (the errors need the context that is
            // dropped when the analysis is reverted).
            let diagnostics = analysis.take_diagnostics();
            let is_ready = diagnostics.is_empty();
            status = status.compose(use_pipeline(
                &name,
                analysis.attributed_id(),
                analysis.analyzer(),
                &mut pipeline.vm,
                diagnostics,
                &mut importer,
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
            let diagnostics = pipeline.analyzer.take_diagnostics();
            status = status.compose(use_pipeline(
                &name,
                SourceId(0), // this value has no importance
                &pipeline.analyzer,
                &mut pipeline.vm,
                diagnostics,
                &mut importer,
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
