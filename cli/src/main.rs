mod cli;
mod repl;
mod report;

use crate::cli::Cli;
use crate::repl::prompt;
use crate::report::FormattedParseError;
use clap::Parser;
use context::source::Source;
use dbg_pls::color;
use miette::{MietteHandlerOpts};
use parser::parse;
use std::io;
use std::process::exit;

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    miette::set_hook(Box::new(|_| {
        Box::new(MietteHandlerOpts::new()
            .tab_width(2)
            .build())
    })).expect("miette setup");

    if let Some(source) = cli.source {
        let content = std::fs::read_to_string(&source)?;
        let name = source.to_string_lossy();
        let source = Source::new(&content, &name);
        let report = parse(source);
        let errors = report
            .errors
            .into_iter()
            .map(|err| FormattedParseError::from(err, &source))
            .collect::<Vec<_>>();

        if errors.is_empty() {
            eprintln!("{}", color(&report.expr));
            return Ok(());
        }
        for err in &errors {
            eprintln!("{err:?}")
        }
        exit(1);
    }
    prompt();
    Ok(())
}
