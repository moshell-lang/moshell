use std::io::stderr;
use std::path::PathBuf;

use clap::Parser;

use analyzer::engine::Engine;
use analyzer::importer::ASTImporter;
use analyzer::name::Name;
use analyzer::resolve_all;
use analyzer::steps::typing::apply_types;
use analyzer::types::engine::TypedEngine;
use analyzer::types::Typing;
use compiler::compile;

use crate::pipeline::{ErrorReporter, FileImportError};
use crate::report::{display_diagnostic, display_parse_error};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Defines the source file to parse
    #[arg(short, long, value_name = "FILE")]
    pub(crate) source: Option<PathBuf>,
}

pub fn resolve_and_execute<'a>(
    entry_point: Name,
    importer: &mut (impl ASTImporter<'a> + ErrorReporter),
) -> bool {
    let result = resolve_all(entry_point, importer);

    let errors = importer.take_errors();
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

    let mut diagnostics = result.diagnostics;
    if diagnostics.is_empty() {
        let (types, typing) = apply_types(&result.engine, &result.relations, &mut diagnostics);
        if diagnostics.is_empty() {
            execute(types, result.engine, typing);
            return false;
        }
    }

    let mut stdout = stderr();
    let had_errors = !diagnostics.is_empty();
    for diagnostic in diagnostics {
        let source = importer
            .get_source(diagnostic.source)
            .expect("Unknown source");
        display_diagnostic(source, diagnostic, &mut stdout)
            .expect("IO errors when reporting diagnostic")
    }
    had_errors
}

#[link(name = "vm", kind = "static")]
extern "C" {
    /// Execute the given bytecode.
    ///
    /// # Safety
    /// If the given bytecode is invalid, this function will cause undefined behavior.
    fn moshell_exec(bytes: *const u8, byte_count: usize);
}

fn execute(types: TypedEngine, engine: Engine, typing: Typing) {
    let mut bytes: Vec<u8> = Vec::new();
    compile(&types, &engine, &typing, &mut bytes).expect("write failed");

    let len = bytes.len();

    unsafe {
        moshell_exec(bytes.as_ptr(), len);
    }
}
