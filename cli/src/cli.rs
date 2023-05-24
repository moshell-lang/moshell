use std::collections::HashMap;
use std::io::stderr;
use std::path::PathBuf;

use colored::{Colorize};
use crate::report::{display_diagnostic, display_parse_error};
use analyzer::engine::Engine;
use analyzer::importer::ASTImporter;
use analyzer::name::Name;
use analyzer::relations::Relations;
use analyzer::steps::collect::SymbolCollector;
use analyzer::steps::resolve::SymbolResolver;
use ast::group::Block;
use lexer::lexer::lex;
use lexer::token::Token;
use ast::Expr;
use clap::Parser;
use context::source::Source;
use dbg_pls::color;
use parser::parse;

#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Defines the source file to evaluate
    #[arg(short, long, value_name = "FILE")]
    pub(crate) source: Option<PathBuf>,

    /// Activate lexer output visualisation
    #[arg(short = 'L', long)]
    pub(crate) lexer: bool,

    /// Activate parser AST visualisation
    #[arg(short = 'P', long)]
    pub(crate) parser: bool,

    /// Activate analyzer visualisation
    #[arg(short = 'A', long, default_value = "true")]
    pub(crate) analyzer: bool,
}


pub struct Configuration {
    pub lexer_visualisation: bool,
    pub parser_visualization: bool,
    pub analyzer: bool,
}

impl From<Cli> for Configuration {
    fn from(value: Cli) -> Self {
        Self {
            lexer_visualisation: value.lexer,
            parser_visualization: value.parser,
            analyzer: value.parser,
        }
    }
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

fn display_tokens(tokens: Vec<Token>) {
    println!("{}", "Lexer tokens: ".bright_black());
    println!("{}", format!("\t- {} tokens lexed from input", tokens.len()));

    let mut count = 0;
    for token in tokens {
        print!("{:10}: '{}', ", format!("{:?}", token.token_type).bright_blue(), token.value.white());
        count += 1;
        if count % 5 == 0 {
            println!()
        }
    }
    println!()
}

fn display_exprs(exprs: Vec<Expr>) {
    for expr in exprs {
        println!("{}", color(&expr));
    }
}

/// Parses and display errors / diagnostics coming from the given source.
/// Returning true if the source had at least one error or diagnostic.
pub fn handle_source(source: Source, config: &Configuration) -> bool {
    if config.lexer_visualisation {
        display_tokens(lex(&source.source))
    }

    let report = parse(source);
    let mut importer = RawImporter::default();

    let mut engine = Engine::default();
    let mut relations = Relations::default();

    let source = source;
    let errors: Vec<_> = report.errors;

    let out = &mut stderr();
    if !errors.is_empty() {
        for error in errors {
            display_parse_error(source, error, out).expect("IO error when reporting diagnostics");
        }
        return true;
    }

    if config.parser_visualization {}

    let expr = Expr::Block(Block {
        expressions: report.expr,
        segment: 0..0,
    });

    let name = Name::new("<module>");
    importer.imported_modules.insert(name.clone(), expr);

    let mut diagnostics =
        SymbolCollector::collect_symbols(&mut engine, &mut relations, name, &mut importer);
    diagnostics.extend(SymbolResolver::resolve_symbols(&engine, &mut relations));

    let mut stdout = stderr();
    let had_errors = !diagnostics.is_empty();
    for diagnostic in diagnostics {
        display_diagnostic(source, diagnostic, &mut stdout)
            .expect("IO errors when reporting diagnostic")
    }
    had_errors
}
