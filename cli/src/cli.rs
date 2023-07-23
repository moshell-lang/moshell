use std::io::stderr;
use std::path::PathBuf;

use clap::Parser;
use dbg_pls::color;

use analyzer::importer::ASTImporter;
use analyzer::name::Name;
use analyzer::resolve_all;
use analyzer::steps::typing::apply_types;
use compiler::compile;
use vm::execute_bytecode;

use crate::disassemble::display_bytecode;
use crate::pipeline::{ErrorReporter, FileImportError};
use crate::report::{display_diagnostic, display_parse_error};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Defines the source file to parse
    #[arg(short, long, value_name = "FILE")]
    pub(crate) source: Option<PathBuf>,

    /// Prints the generated bytecode
    #[arg(short = 'D', long)]
    pub(crate) disassemble: bool,

    /// Display a textual representation of the abstract syntax tree
    #[arg(short = 'A', long)]
    pub(crate) ast: bool,

    /// Do not execute the code
    #[arg(long = "no-execute")]
    pub(crate) no_execute: bool,
}

pub fn resolve_and_execute<'a>(
    entry_point: Name,
    importer: &mut (impl ASTImporter<'a> + ErrorReporter),
    config: &Cli,
) -> bool {
    let result = resolve_all(entry_point.clone(), importer);

    let errors = importer.take_errors();
    if errors.is_empty() && result.engine.is_empty() {
        eprintln!("No module found for entry point {entry_point}");
        return true;
    }

    let has_errors = !errors.is_empty();
    for error in errors {
        match error {
            FileImportError::IO(err) => {
                eprintln!("IO error: {err}");
            }
            FileImportError::Parse(report) => {
                for error in report.errors {
                    display_parse_error(
                        importer.get_source(report.source).unwrap(),
                        error,
                        &mut stderr(),
                    )
                    .expect("IO error when reporting diagnostics");
                }
            }
        }
    }
    if has_errors {
        return true;
    }

    if config.ast {
        for ast in result
            .engine
            .environments()
            .filter(|(_, env)| env.parent.is_none())
            .filter_map(|(id, _)| result.engine.get_expression(id))
        {
            println!("{}", color(ast))
        }
    }

    let mut diagnostics = result.diagnostics;
    if diagnostics.is_empty() {
        let (types, typing) = apply_types(&result.engine, &result.relations, &mut diagnostics);
        if diagnostics.is_empty() {
            let mut bytes = Vec::new();
            compile(&types, &result.engine, &typing, &mut bytes).expect("write failed");

            if config.disassemble {
                display_bytecode(&bytes);
            }

            if !config.no_execute {
                execute(&bytes);
            }

            return false;
        }
    }

    let mut stderr = stderr();
    let had_errors = !diagnostics.is_empty();
    for diagnostic in diagnostics {
        display_diagnostic(&result.engine, importer, diagnostic, &mut stderr)
            .expect("IO errors when reporting diagnostic");
    }
    had_errors
}

fn execute(bytes: &[u8]) {
    unsafe {
        execute_bytecode(bytes);
    }
}
