use std::io;
use std::io::stderr;
use std::ops::Deref;
use std::path::PathBuf;

use analyzer::diagnostic::Diagnostic;
use analyzer::engine::Engine;
use analyzer::importer::ASTImporter;
use analyzer::name::Name;
use analyzer::relations::{Relations, SourceObjectId};
use analyzer::steps::collect::SymbolCollector;
use analyzer::steps::resolve::SymbolResolver;
use ast::Expr;
use ast::group::Block;
use context::source::{Source, SourceSegmentHolder};
use lexer::lexer::lex;
use parser::err::ParseReport;
use parser::parse;

use crate::cli::{Configuration, display_exprs, display_tokens};
use crate::report::{display_diagnostic, display_parse_error};
use crate::source_importer::FileSourceImporter;

pub(crate) fn run(source: PathBuf, working_dir: PathBuf, config: Configuration) -> bool {
    let name = source.to_string_lossy();
    let mut name = name.deref().replace("/", "::");
    if let Some((name_no_extension, _)) = name.rsplit_once(".") {
        name = name_no_extension.to_string()
    }
    let entry_point = Name::new(&name);

    let mut engine = Engine::default();
    let mut relations = Relations::default();
    let mut importer = RunnerImporter::new(working_dir);

    let mut diagnostics = SymbolCollector::collect_symbols(&mut engine, &mut relations, entry_point, &mut importer);
    diagnostics.extend(SymbolResolver::resolve_symbols(&engine, &mut relations));

    if !importer.errors.is_empty() {
        display_import_errors(importer.errors);
        return true
    }

    let had_errors = diagnostics.is_empty();
    display_diagnostics(diagnostics, &engine, &importer);
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
            .expect(&format!(
                "Could not retrieve source of environment {}",
                env_id.0
            ));
    }
}


fn display_import_errors(errors: Vec<RunnerImporterError>) {
    for parse_error in errors {
        match parse_error {
            RunnerImporterError::IO(name, e) => {
                eprintln!("IO error: could not import {name}: {e}")
            }
            RunnerImporterError::Parse(source, report) => {
                for err in report.errors {
                    display_parse_error(source, err, &mut stderr()).expect("Could not display parse error");
                }
            }
        }
    }
}

fn display_diagnostics(diagnostics: Vec<Diagnostic>, engine: &Engine, importer: &RunnerImporter) {
    let stderr = &mut stderr();

    for diagnostic in diagnostics {
        let source = get_source_of(diagnostic.source, engine, importer);
        display_diagnostic(source, diagnostic, stderr).expect("could not write in stderr");
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
            self.errors
                .push(RunnerImporterError::Parse(source, report));
            return None;
        }
        let block = Expr::Block(Block {
            expressions: report.expr,
            segment: source.segment(),
        });
        Some(block)
    }
}
