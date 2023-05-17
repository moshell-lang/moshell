use std::collections::{HashMap, VecDeque};
use crate::report::{print_flush, FormattedParseError};
use context::source::Source;
use dbg_pls::color;
use miette::GraphicalReportHandler;
use parser::parse;
use std::io;
use std::io::{BufRead};
use std::io::Write;
use analyzer::diagnostic::Diagnostic;
use analyzer::engine::Engine;
use analyzer::importer::ASTImporter;
use analyzer::name::Name;
use analyzer::relations::Relations;
use analyzer::steps::collect::SymbolCollector;
use analyzer::steps::resolve::SymbolResolver;
use ast::Expr;
use ast::group::Block;
use parser::err::ParseReport;


pub fn prompt(handler: GraphicalReportHandler) {
    let mut importer = REPLImporter {
        stdin_expressions: VecDeque::new(),
        imported_expressions: HashMap::new(),
    };

    loop {
        let mut engine = Engine::default();
        let mut relations = Relations::default();
        let mut content = String::new();
        let result = parse_input(&mut content);

        if result.is_none() {
            continue
        }
        let (report, source) = result.unwrap();

        let errors: Vec<_> = report
            .errors
            .into_iter()
            .map(|err| FormattedParseError::from(err, &source))
            .collect();

        if !errors.is_empty() {
            display_parse_errors(errors, &handler);
            content.clear();
            print_flush!("=> ");
            return;
        }

        print_flush!("{}\n=> ", color(&report.expr));
        content.clear();
        let expr = Expr::Block(Block {
            expressions: report.expr,
            segment: 0..0,
        });

        importer.stdin_expressions.push_front(expr);

        let mut diagnostics = SymbolCollector::collect_symbols(&mut engine, &mut relations, Name::new("stdin"), &mut importer);
        diagnostics.extend(SymbolResolver::resolve_symbols(&engine, &mut relations));

        for diagnostic in diagnostics {
            display_diagnostic(diagnostic, &handler, &engine)
        }
    }
}

fn parse_input(content: &mut String) -> Option<(ParseReport, Source)> {
    let stdin = io::stdin();
    let lines = stdin.lock().lines();

    print_flush!("=> ");
    for line in lines {
        let line = line.expect("couldn't read line from stdin");
        content.push_str(&line);
        if line.ends_with('\\') {
            //content.push('\n');
            print_flush!(".. ");
            continue;
        }

        let source = Source::new(&content, "stdin");
        let report = parse(source);
        if !report.stack_ended {
            drop(source);
            //content.push('\n');
            print_flush!(".. ");
            continue; // Silently ignore incomplete input
        }

        return Some((report, source))
    }
    None
}

fn display_diagnostic(diagnostic: Diagnostic, handler: &GraphicalReportHandler, engine: &Engine) {}

fn display_parse_errors(errors: Vec<FormattedParseError>, handler: &GraphicalReportHandler) {
    let mut msg = String::new();
    for err in &errors {
        if let Err(fmt_err) = handler.render_report(&mut msg, err) {
            eprintln!("{fmt_err}");
        } else {
            eprintln!("{msg}");
        }
        msg.clear();
    }
}

struct REPLImporter<'a> {
    imported_expressions: HashMap<Name, Expr<'a>>,

    stdin_expressions: VecDeque<Expr<'a>>,
}

impl<'a> ASTImporter<'a> for REPLImporter<'a> {
    fn import(&mut self, name: &Name) -> Option<Expr<'a>> {
        if name == &Name::new("stdin") {
            return self.stdin_expressions.pop_back()
        }
        self.imported_expressions.get(name).cloned()
    }
}