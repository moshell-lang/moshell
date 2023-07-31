use std::io;
use std::io::BufRead;
use std::io::Write;

use analyzer::importer::ImportResult;
use analyzer::name::Name;
use analyzer::relations::SourceId;
use analyzer::Inject;
use context::source::{OwnedSource, Source};
use parser::parse;

use crate::cli::{use_pipeline, Cli};
use crate::pipeline::{FileImporter, Pipeline};
use crate::report::print_flush;

/// Indefinitely prompts a new expression from stdin and executes it.
pub fn prompt(mut importer: FileImporter, config: &Cli) {
    let mut pipeline = Pipeline::new();
    let mut starting_source: Option<SourceId> = None;
    while let Some(source) = parse_input() {
        let name = Name::new("stdin");
        if let ImportResult::Success(imported) = importer.insert(source) {
            let mut analysis = pipeline.analyzer.inject(
                Inject {
                    name: name.clone(),
                    imported,
                    attached: starting_source,
                },
                &mut importer,
            );

            let diagnostics = analysis.take_diagnostics();
            let is_ready = diagnostics.is_empty();
            use_pipeline(
                name,
                analysis.attributed_id(),
                analysis.analyzer(),
                &mut pipeline.vm,
                diagnostics,
                &mut importer,
                config,
            );
            if is_ready {
                starting_source = Some(analysis.attributed_id());
            } else {
                analysis.revert();
            }
        } else {
            let diagnostics = pipeline.analyzer.take_diagnostics();
            use_pipeline(
                name,
                SourceId(0), // this value has no importance
                &pipeline.analyzer,
                &mut pipeline.vm,
                diagnostics,
                &mut importer,
                config,
            );
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

        return Some(OwnedSource::new(
            source.source.to_string(),
            source.name.to_string(),
        ));
    }
    None
}
