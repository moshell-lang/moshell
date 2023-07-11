#![allow(dead_code)]
mod cli;
mod disassemble;
mod repl;
mod report;

use crate::cli::{handle_source, Cli};
use crate::repl::prompt;
use clap::Parser;
use context::source::Source;
use miette::MietteHandlerOpts;
use std::io;
use std::ops::Deref;
use std::process::exit;

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    miette::set_hook(Box::new(|_| {
        Box::new(MietteHandlerOpts::new().tab_width(2).build())
    }))
    .expect("miette options setup");

    if let Some(source) = &cli.source {
        let content = std::fs::read_to_string(source)?;
        let name = source.to_string_lossy().deref().to_string();
        let source = Source::new(&content, &name);
        exit(handle_source(&cli, source) as i32)
    }
    prompt(&cli);
    Ok(())
}
