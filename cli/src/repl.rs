use std::collections::{HashMap, VecDeque};
use crate::report::{print_flush, FormattedParseError, display_diagnostic};
use context::source::OwnedSource;
use dbg_pls::color;
use parser::parse;
use std::io;
use std::io::{BufRead, stderr};
use std::io::Write;
use analyzer::engine::Engine;
use analyzer::importer::ASTImporter;
use analyzer::name::Name;
use analyzer::relations::Relations;
use analyzer::steps::collect::SymbolCollector;
use analyzer::steps::resolve::SymbolResolver;
use ast::Expr;
use ast::group::Block;
use parser::err::ParseReport;


pub fn prompt() {
    loop {
        if let Some((report, source)) = parse_input() {
            handle_output(report, source)
        }
    }
}

fn handle_output(report: ParseReport, source: OwnedSource) {
    let mut importer = REPLImporter {
        stdin_expressions: VecDeque::new(),
        imported_expressions: HashMap::new(),
    };

    let mut engine = Engine::default();
    let mut relations = Relations::default();

    let source = source.as_source();
    let errors: Vec<_> = report
        .errors
        .into_iter()
        .map(|err| FormattedParseError::from(err, &source))
        .collect();

    if !errors.is_empty() {
        display_parse_errors(errors);
        print_flush!("=> ");
        return;
    }

    let expr = Expr::Block(Block {
        expressions: report.expr,
        segment: 0..0,
    });

    println!("{}", color(&expr));

    importer.stdin_expressions.push_front(expr);

    let mut diagnostics = SymbolCollector::collect_symbols(&mut engine, &mut relations, Name::new("stdin"), &mut importer);
    diagnostics.extend(SymbolResolver::resolve_symbols(&engine, &mut relations));

    let mut stdout = stderr();
    for diagnostic in diagnostics {
        display_diagnostic(source, diagnostic, &mut stdout)
            .expect("IO errors when reporting diagnostic")
    }
}

fn parse_input<'a>() -> Option<(ParseReport<'a>, OwnedSource)> {
    let stdin = io::stdin();
    let lines = stdin.lock().lines();
    let mut content = String::new();
    print_flush!("=> ");
    for line in lines {
        let line = line.expect("couldn't read line from stdin");
        content.push_str(&line);
        if line.ends_with('\\') {
            content.push('\n');
            print_flush!(".. ");
            continue;
        }

        let source = OwnedSource::new(content.clone(), "stdin".to_string());
        let report = parse(source.as_source());
        if !report.stack_ended {
            content.push('\n');
            print_flush!(".. ");
            continue; // Silently ignore incomplete input
        }

        return unsafe {
            // SAFETY: The owned source of the ParseReport is returned,
            // thus the expression's slices are not dropped
            Some((std::mem::transmute::<ParseReport, ParseReport<'a>>(report), source))
        };
    }
    None
}


fn display_parse_errors(errors: Vec<FormattedParseError>) {
    for err in &errors {
        eprintln!("{err:?}")
    }
}

struct REPLImporter<'a> {
    imported_expressions: HashMap<Name, Expr<'a>>,

    stdin_expressions: VecDeque<Expr<'a>>,
}

impl<'a> ASTImporter<'a> for REPLImporter<'a> {
    fn import(&mut self, name: &Name) -> Option<Expr<'a>> {
        if name == &Name::new("stdin") {
            return self.stdin_expressions.pop_back();
        }
        self.imported_expressions.get(name).cloned()
    }
}