use std::io;
use std::io::stderr;
use std::ops::Deref;
use std::path::PathBuf;

use analyzer::diagnostic::Diagnostic;
use analyzer::engine::Engine;
use analyzer::importer::ASTImporter;
use analyzer::name::Name;
use analyzer::relations::SourceId;
use analyzer::steps::typing::apply_types;
use ast::group::Block;
use ast::Expr;
use compiler::compile;
use context::source::{Source, SourceSegmentHolder};
use lexer::lexer::lex;
use parser::err::ParseReport;
use parser::parse;

use crate::cli::{display_exprs, display_tokens, execute, CliConfiguration};
use crate::disassemble::display_bytecode;
use crate::render::{render_diagnostic, render_parse_error, SourcesCache};
use crate::source_importer::FileSourceImporter;

/// returns true if no error occurred
pub(crate) fn run(source: PathBuf, source_dir: PathBuf, config: CliConfiguration) -> bool {
    let name = source.to_string_lossy();
    let mut name = name.deref().replace('/', "::");
    if let Some((name_no_extension, _)) = name.rsplit_once('.') {
        name = name_no_extension.to_string()
    }
    let entry_point = Name::new(&name);

    let mut importer = RunnerImporter::new(source_dir);

    let result = analyzer::resolve_all(entry_point, &mut importer);

    let mut diagnostics = result.diagnostics;
    let engine = result.engine;
    let relations = result.relations;

    if !importer.errors.is_empty() {
        display_import_errors(importer.errors);
        return false;
    }

    let typed_engine = apply_types(&engine, &relations, &mut diagnostics);

    if !diagnostics.is_empty() {
        display_diagnostics(diagnostics, &engine, &importer);
        return false;
    }

    let mut bytecode = Vec::new();
    let root_expr = &typed_engine.get_user(SourceId(0)).unwrap().expression;
    compile(root_expr, &mut bytecode).unwrap();

    visualize_outputs(importer, &config, &bytecode);

    if config.execute {
        execute(&bytecode);
    }

    true
}

fn visualize_outputs(importer: RunnerImporter, config: &CliConfiguration, bytecode: &[u8]) {
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

    if config.bytecode_visualisation {
        display_bytecode(bytecode)
    }

    println!("End of visualisation.");
    println!("---------------------");
}

fn get_source_of<'a>(
    mut env_id: SourceId,
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

fn display_import_errors(errors: Vec<RunnerImporterError>) {
    for parse_error in errors {
        match parse_error {
            RunnerImporterError::IO(name, e) => {
                eprintln!("IO error: could not import {name}: {e}")
            }
            RunnerImporterError::Parse(source, report) => {
                for err in report.errors {
                    render_parse_error(source, err, &mut stderr())
                        .expect("Could not display parse error");
                }
            }
        }
    }
}

fn display_diagnostics(diagnostics: Vec<Diagnostic>, engine: &Engine, importer: &RunnerImporter) {
    let mut cache = SourcesCache::new(|src| get_source_of(src, engine, importer));
    for diagnostic in diagnostics {
        render_diagnostic(diagnostic, &mut cache, &mut stderr())
            .expect("could not write in stderr");
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
            Ok(src) => self.parse_source(src),
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

    fn parse_source(&mut self, source: Source<'a>) -> Option<Expr<'a>> {
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
