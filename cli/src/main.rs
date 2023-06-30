#![allow(dead_code)]

use std::io;
use std::process::exit;

use clap::Parser;
use miette::MietteHandlerOpts;

use analyzer::name::Name;

use crate::cli::{Cli, resolve_and_execute};
use crate::pipeline::FileImporter;
use crate::repl::prompt;

mod cli;
mod pipeline;
mod repl;
mod report;

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    miette::set_hook(Box::new(|_| {
        Box::new(MietteHandlerOpts::new().tab_width(2).build())
    }))
    .expect("miette options setup");

    let mut importer = FileImporter::new(std::env::current_dir()?);
    if let Some(source) = cli.source {
        let name = Name::new(
            source
                .file_name()
                .and_then(|name| name.to_str())
                .expect("Incompatible filename"),
        );
        let has_error = resolve_and_execute(name, &mut importer);
        exit(i32::from(has_error))
    }
    prompt(importer);
    Ok(())
}
