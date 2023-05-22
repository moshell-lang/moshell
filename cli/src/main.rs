#![allow(dead_code)]
#![deny(warnings)]
mod cli;
mod repl;
mod report;

use crate::cli::{handle_source, Cli};
use crate::repl::prompt;
use clap::Parser;
use context::source::OwnedSource;
use miette::MietteHandlerOpts;
use std::io;
use std::ops::Deref;
use std::process::exit;

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    miette::set_hook(Box::new(|_| {
        Box::new(MietteHandlerOpts::new().tab_width(2).build())
    }))
    .expect("miette setup");

    if let Some(source) = cli.source {
        let content = std::fs::read_to_string(&source)?;
        let name = source.to_string_lossy().deref().to_string();
        let source = OwnedSource::new(content, name);
        exit(handle_source(source) as i32)
    }
    prompt();
    Ok(())
}
