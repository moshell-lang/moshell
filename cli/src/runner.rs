use std::io;
use std::ops::Deref;
use std::path::PathBuf;

use miette::GraphicalReportHandler;

use analyzer::diagnostic::Diagnostic;
use analyzer::engine::Engine;
use analyzer::importer::ASTImporter;
use analyzer::name::Name;
use analyzer::relations::SourceObjectId;
use ast::group::Block;
use ast::Expr;
use context::source::{Source, SourceSegmentHolder};
use lexer::lexer::lex;
use parser::err::ParseReport;
use parser::parse;

use crate::cli::{display_exprs, display_tokens, Configuration};
use crate::formatted_diagnostic::render_diagnostic;
use crate::formatted_parse_error::render_parse_error;
use crate::source_importer::FileSourceImporter;

pub(crate) fn run(
    source: PathBuf,
    working_dir: PathBuf,
    config: Configuration,
    handler: GraphicalReportHandler,
) -> bool {
    let name = source.to_string_lossy();
    let mut name = name.deref().replace('/', "::");
    if let Some((name_no_extension, _)) = name.rsplit_once('.') {
        name = name_no_extension.to_string()
    }
    let entry_point = Name::new(&name);

    let mut importer = RunnerImporter::new(working_dir);

    let result = analyzer::analyze_all(entry_point, &mut importer);

    let diagnostics = result.diagnostics;
    let engine = result.engine;

    if !importer.errors.is_empty() {
        display_import_errors(importer.errors, &handler);
        return true;
    }

    let had_errors = diagnostics.is_empty();
    display_diagnostics(diagnostics, &engine, &importer, &handler);
    visualize_outputs(importer, config);
    had_errors
}

fn visualize_outputs(importer: RunnerImporter, config: Configuration) {
    if !config.needs_visualisation() {
        return;
    }
    for source in importer.source_importer.list_sources() {
        let name = source.name;
        println!("{name}: ");
        if config.parser_visualization {
            display_tokens(lex(source.source))
        }
        if config.parser_visualization {
            display_exprs(&parse(source).expr)
        }
    }
}

fn get_source_of<'a>(
    mut env_id: SourceObjectId,
    engine: &Engine<'a>,
    importer: &RunnerImporter,
) -> Source<'a> {
    loop {
        let env = engine
            .get_environment(env_id)
            .expect("Diagnostic points to an unknown environment.");
        if let Some(parent_id) = env.parent {
            env_id = parent_id;
            continue;
        }
        return importer
            .source_importer
            .get_already_imported_name(&env.fqn)
            .unwrap_or_else(|| panic!("Could not retrieve source of environment {}", env_id.0));
    }
}

fn display_import_errors(errors: Vec<RunnerImporterError>, handler: &GraphicalReportHandler) {
    for parse_error in errors {
        match parse_error {
            RunnerImporterError::IO(name, e) => {
                eprintln!("IO error: could not import {name}: {e}")
            }
            RunnerImporterError::Parse(source, report) => {
                for err in report.errors {
                    let str = render_parse_error(err, handler, source)
                        .expect("Could not display parse error");
                    eprintln!("{str}")
                }
            }
        }
    }
}

fn display_diagnostics(
    diagnostics: Vec<Diagnostic>,
    engine: &Engine,
    importer: &RunnerImporter,
    handler: &GraphicalReportHandler,
) {
    for diagnostic in diagnostics {
        let source = get_source_of(diagnostic.source, engine, importer);
        let str =
            render_diagnostic(source, diagnostic, handler).expect("could not write in stderr");
        eprintln!("{str}")
    }
}

enum RunnerImporterError<'a> {
    IO(Name, io::Error),
    Parse(Source<'a>, ParseReport<'a>),
}

struct RunnerImporter<'a> {
    source_importer: FileSourceImporter,
    errors: Vec<RunnerImporterError<'a>>,
}

impl<'a> ASTImporter<'a> for RunnerImporter<'a> {
    fn import(&mut self, name: &Name) -> Option<Expr<'a>> {
        match self.source_importer.import_source(name) {
            Ok(src) => self.import_expr(src),
            Err(e) => {
                self.errors.push(RunnerImporterError::IO(name.clone(), e));
                None
            }
        }
    }
}

impl<'a> RunnerImporter<'a> {
    fn new(root: PathBuf) -> Self {
        Self {
            source_importer: FileSourceImporter::new(root),
            errors: Vec::new(),
        }
    }

    fn import_expr(&mut self, source: Source<'a>) -> Option<Expr<'a>> {
        let report = parse(source);
        if report.is_err() {
            self.errors.push(RunnerImporterError::Parse(source, report));
            return None;
        }
        let block = Expr::Block(Block {
            expressions: report.expr,
            segment: source.segment(),
        });
        Some(block)
    }
}
