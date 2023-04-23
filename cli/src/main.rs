mod cli;
mod repl;
mod report;

use crate::cli::Cli;
use crate::repl::prompt;
use crate::report::FormattedError;
use clap::Parser;
use context::source::Source;
use dbg_pls::color;
use miette::GraphicalReportHandler;
use parser::parse;
use std::io;
use std::process::exit;

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    let handler = GraphicalReportHandler::default();

    if let Some(source) = cli.source {
        let content = std::fs::read_to_string(&source)?;
        let source = Source::new(&content, source.to_string_lossy().as_ref());
        let report = parse(source.clone());
        let errors = report
            .errors
            .into_iter()
            .map(|err| FormattedError::from(err, &source))
            .collect::<Vec<_>>();
        if errors.is_empty() {
            println!("{}", color(&report.expr));
            return Ok(());
        }
        let mut msg = String::new();
        for err in &errors {
            if let Err(fmt_err) = handler.render_report(&mut msg, err) {
                eprintln!("{fmt_err}");
                msg.clear();
            }
        }
        if !msg.is_empty() {
            eprintln!("{msg}");
        }
        exit(1);
    }
    prompt(handler)
}
