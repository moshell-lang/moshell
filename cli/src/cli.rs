use std::collections::HashMap;
use std::io::stderr;
use std::path::PathBuf;

use clap::Parser;
use dbg_pls::color;

use analyzer::resolve_all;
use analyzer::importer::ASTImporter;
use analyzer::name::Name;
use ast::group::Block;
use ast::Expr;
use context::source::Source;
use parser::parse;

use crate::report::{display_diagnostic, display_parse_error};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Defines the source file to parse
    #[arg(short, long, value_name = "FILE")]
    pub(crate) source: Option<PathBuf>,
}

#[derive(Default)]
struct RawImporter<'a> {
    imported_modules: HashMap<Name, Expr<'a>>,
}

impl<'a> ASTImporter<'a> for RawImporter<'a> {
    fn import(&mut self, name: &Name) -> Option<Expr<'a>> {
        self.imported_modules.get(name).cloned()
    }
}

/// Parses and display errors / diagnostics coming from the given source.
/// Returning true if the source had at least one error or diagnostic.
pub fn handle_source(source: Source) -> bool {
    let report = parse(source);
    let mut importer = RawImporter::default();

    let source = source;
    let errors: Vec<_> = report.errors;

    let out = &mut stderr();
    if !errors.is_empty() {
        for error in errors {
            display_parse_error(source, error, out).expect("IO error when reporting diagnostics");
        }
        return true;
    }

    println!("{}", color(&report.expr));

    let expr = Expr::Block(Block {
        expressions: report.expr,
        segment: 0..0,
    });

    let name = Name::new("<module>");
    importer.imported_modules.insert(name.clone(), expr);

    let result = resolve_all(name, &mut importer);

    let diagnostics = result.diagnostics;

    let mut stdout = stderr();
    let had_errors = !diagnostics.is_empty();
    for diagnostic in diagnostics {
        display_diagnostic(source, diagnostic, &mut stdout)
            .expect("IO errors when reporting diagnostic")
    }
    had_errors
}
